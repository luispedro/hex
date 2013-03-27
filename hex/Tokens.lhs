\section{Tokens}

This module handles Token parsing. It transforms a char stream into a token stream

\begin{code}
module Tokens
    ( Token(..)
    , tokenCategory
    , TokenStream(..)
    , newTokenStream
    , gettoken
    , tokenliststream
    , emptyTokenStream
    , updateCharStream
    , toksToStr
    ) where

import Chars
import CharStream

import Data.Maybe
\end{code}

Tokens are the next level after annotated characters. A Token is either a
control sequence or an annotated character.

\begin{code}
data Token =
    ControlSequence String
    | CharToken TypedChar
    deriving (Eq)

instance Show Token where
    show (ControlSequence name) = "[" ++ name ++ "]"
    show (CharToken tc) = show tc
\end{code}

Sometimes, just the category of a character is important.
\begin{code}
tokenCategory (CharToken tc) = category tc
tokenCategory _ = Invalid -- It doesn't matter what
\end{code}

We implement the state system described in pp. 46--47 of the Texbook. The 3
states, N, S, and M, are mapped to 3 functions sN, sM, and sS. The
implementation is a bit simpler that what is described in the book. In
particular, there is no concept of ``lines'' separate from the separation by
newline characters. Whether this will matter, only time will tell.

We start with \code{skiptoeol} which is used for comments. It skips everything,
up to, and including, a newline.

\begin{code}
skiptoeol :: TypedCharStream -> TypedCharStream
skiptoeol st = case getchar st of
    Nothing -> st
    Just (c,st')
        | category c == EOL -> st'
        | otherwise -> skiptoeol st'
\end{code}

In order to have the state functions return themselves, we need to define them
as a datatype. Unfortunately, we also need to define the ugly
\code{applyStateFunction} helper.

\begin{code}
newtype StateFunction = StateFunction { applyStateFunction :: TypedCharStream -> Maybe (Token, TypedCharStream, StateFunction) }
\end{code}

Now, the implementation of the two easy states: S \& N. N is the Newline state.
We transform another newline into \tex{\\par} and otherwise ignore spaces.

\begin{code}
sN = StateFunction sN' where
    sN' st = case getchar st of
        Nothing -> Nothing
        Just (c,rest)
            | category c == EOL -> Just (ControlSequence "\\par", rest, sN)
            | category c == Space -> sN' rest
            | otherwise -> applyStateFunction sM st
\end{code}

S is the space state. Multiple spaces get collapsed here as we ignore them
until we get a character.

\begin{code}
sS = StateFunction sS' where
    sS' st = case getchar st of
        Nothing -> Nothing
        Just (c,rest)
            | category c == EOL -> applyStateFunction sN rest
            | category c == Space -> applyStateFunction sS rest
            | otherwise -> applyStateFunction sM st
\end{code}

\code{sM} does most of the real work of transforming \code{TypedChar}s into
\code{Token}s. This is the M state: middle of a line.

Almost everything is a \code{CharToken}, except single non-letters or a word
following an \code{Escape}. A word is a sequence of \code{Letter}s followed by
non-\code{Letter}. Therefore, getting a csname is done in two steps: if it is
single character, just stop there; otherwise, invoke \code{breakup'}.

The rule of what the next state is are a bit convoluted, but they're taken from
the Texbook.

\begin{code}

sM = StateFunction sM' where
    sM' st = case getchar st of
        Nothing -> Nothing
                                -- Maybe below should be (TypedChar '\n' Space) ?
        Just (c,rest)
            | category c == EOL -> Just (CharToken (TypedChar ' ' Space), rest, sN)
            | category c == Space -> Just (CharToken c, rest, sS)
            | category c == Comment -> applyStateFunction sN $ skiptoeol rest
            | category c == Active -> Just (ControlSequence [value c], rest, sM)
            | category c /= Escape -> Just (CharToken c, rest, sM)
            | otherwise -> Just (ControlSequence ('\\':name), rest', nextstate)
            where
                (name, rest', nextstate) = breakup rest
        where
            breakup st' = case getchar st' of
                Nothing -> ([],st',sN)
                Just (c,brest)
                    | category c == Space -> ([value c], brest, sS)
                    | category c /= Letter -> ([value c], brest, sM)
                    | otherwise -> breakup' [] st'
            breakup' acc st' = case getchar st' of
                Nothing -> (acc, st', sS)
                Just (c,brest)
                    | category c /= Letter -> (acc, st', sS)
                    | otherwise -> breakup' (acc ++ [value c]) brest
\end{code}

Similar to \code{TypedCharStream}, we define a \code{TokenStream} which
transforms the typed chars into \code{Token}s. The biggest difference is the
\code{queue} functionality, which allows one to push back tokens into the
queue.

\begin{code}
data TokenStream = TokenStream
                { charsource :: TypedCharStream
                , state :: StateFunction
                , queue :: [Token]
                }
\end{code}

We need an \haskell{Eq} instance for \haskell{TokenStream} because we are using
these inside \haskell{Command}.
\begin{code}
instance Eq TokenStream where
    _ == _ = False
\end{code}

\begin{code}
instance Show TokenStream where
    show (TokenStream _cs _state q) = show _cs ++ show q

newTokenStream :: TypedCharStream -> TokenStream
newTokenStream cs = TokenStream cs sN []
\end{code}

The main function of this module is \code{gettoken}, which returns a pair
\code{(Token, TokenStream)} unless the stream is empty.

If the queue has tokens, return them; otherwise, read from the character
stream:
\begin{code}
gettoken :: TokenStream -> Maybe (Token, TokenStream)
gettoken tSt@TokenStream{queue=(t:ts)} = Just (t,tSt{queue=ts})
gettoken tSt@TokenStream{charsource=st, state=s, queue=[]} = do
    (tk,st',s') <- applyStateFunction s st
    return (tk,tSt{charsource=st',state=s'})
\end{code}


Sometimes we want to have a simple stream that only spits out the tokens we
initialise it with. This is achieved by \code{tokenliststream}:

\begin{code}
tokenliststream :: [Token] -> TokenStream
tokenliststream = TokenStream emptyTCS sN
\end{code}

\code{emptyTokenStream} is surprisingly tricky. In fact, we need to look ahead
to see if there is anything left to read. For example if the input consists of
a few lines of comments, there are still many chars left, but the token stream
will be empty. If the state functions were defined slightly differently (to
always read as much as they could), this function could be simpler, but the
state functions would be potentially more complex.

\begin{code}
emptyTokenStream TokenStream{charsource=st, state=s, queue=q} = null q && (isNothing $ applyStateFunction s st)
\end{code}

Convert a sequence of \code{CharToken} into a \code{String}
\begin{code}
toksToStr :: [Token] -> [Char]
toksToStr = concatMap tos
    where
        tos (CharToken (TypedChar c _)) = [c]
        tos (ControlSequence s) = "\\" ++ s
\end{code}


Finally, we need to provide access to the underlying character stream.
\begin{code}
updateCharStream t@TokenStream{charsource=s} f = t{charsource=f s}
\end{code}

