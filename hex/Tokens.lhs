\section{Tokens}
\begin{code}
module Tokens
    ( Token(..)
    , skiptoeol
    , TokenStream(..)
    , newTokenStream
    , gettoken
    , gettokentil
    , maybespace
    , maybeeq
    , droptoken
    , streampush
    , streamenqueue
    , tokenliststream
    , emptyTokenStream
    , updateCharStream
    , toksToStr
    , chars2tokens
    , TkS(..)
    , emptyTkS
    , gettokenM
    , skiptokenM
    , peektokenM
    , maybespaceM
    , maybeeqM
    ) where

import Chars
import CharStream
import Defaults (plaintexenv)

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

We implement the state system described in pp. 46--47 of the Texbook. The 3
states, N, S, and M, are mapped to 3 functions sN, sM, and sS. The
implementation is a bit simpler that what is described in the book. In
particular, there is no concept of ``lines'' separate from the separation by
newline characters. Whether this will matter, only time will tell.

We start with \code{skiptoeol} which is used for comments. It skips everything,
up to, and including, a newline.

\begin{code}
skiptoeol :: TypedCharStream -> TypedCharStream
skiptoeol st
    | emptyStream st = st
    | (category c) == EOL = st'
    | otherwise = skiptoeol st'
    where (c,st') = getchar st
\end{code}

In order to have the state functions return themselves, we need to define them
as a datatype. Unfortunately, we also need to define the ugly
\code{applyStateFunction} helper.

\begin{code}
newtype StateFunction = StateFunction ( TypedCharStream -> ([Token], TypedCharStream, StateFunction) )
applyStateFunction (StateFunction s) = s
\end{code}

Now, the implementation of the two easy states: S \& N.

\begin{code}
sN = StateFunction sN' where
    sN' st
        | emptyStream st = ([],st,sN)
        | (category c) == EOL = ([ControlSequence "\\par"], rest, sN)
        | (category c) == Space = sN' rest
        | otherwise = applyStateFunction sM st
        where (c,rest) = getchar st
sS = StateFunction sS' where
    sS' st
        | emptyStream st = ([], st, sS)
        | (category c) == EOL = applyStateFunction sN rest
        | (category c) == Space = applyStateFunction sS rest
        | otherwise = applyStateFunction sM st
        where (c,rest) = getchar st
\end{code}

\code{sM} does most of the real work of transforming \code{TypedChar}s into
\code{Token}s.

Almost everything is a \code{CharToken}, except single non-letters or a word
following an \code{Escape}. A word is a sequence of \code{Letter}s followed by
non-\code{Letter}. Therefore, getting a csname is done in two steps: if it is
single character, just stop there; otherwise, invoke \code{breakup'}.

The rule of what the next state is are a bit convoluted, but they're taken from
the Texbook.

\begin{code}

sM = StateFunction sM' where
    sM' st
        | emptyStream st = ([], st, sM)
                                -- Maybe below should be (TypedChar '\n' Space) ?
        | (category c) == EOL = ([CharToken (TypedChar ' ' Space)], rest, sN)
        | (category c) == Space = ([CharToken c], rest, sS)
        | (category c) == Comment = applyStateFunction sN $ skiptoeol rest
        | (category c) == Active = ([ControlSequence [value c]], rest, sM)
        | (category c) /= Escape = ([CharToken c], rest, sM)
        where (c,rest) = getchar st
    sM' st = ([ControlSequence ('\\':name)], rest', nextstate)
        where
            (_,rest) = getchar st
            (name, rest', nextstate) = breakup rest
            breakup st'
                | emptyStream st' = ([], st', sN)
                | (category c) == Space = ([value c], brest, sS)
                | (category c) /= Letter = ([value c], brest, sM)
                | otherwise = breakup' [] st'
                where (c,brest) = getchar st'
            breakup' acc st'
                | emptyStream st' = (acc, st', sS)
                | (category c) == Space = (acc, brest, sS)
                | (category c) /= Letter = (acc, st', sM)
                | otherwise = breakup' (acc ++ [value c]) brest
                where
                    (c,brest) = getchar st'
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

instance Eq TokenStream where
    _ == _ = False

newTokenStream :: TypedCharStream -> TokenStream
newTokenStream cs = TokenStream cs sN []
\end{code}

The main function of this module is \code{gettoken}, which returns a pair
\code{(Token, TokenStream)}. This could easily be modified into a Monadic
interface.

\begin{code}
gettoken st | emptyTokenStream st = error "hex.Tokens.gettoken: empty stream"
gettoken tSt@TokenStream{queue=(t:ts)} = (t,tSt{queue=ts})
gettoken tSt@TokenStream{charsource=st, state=s, queue=[]} =
    gettoken tSt{charsource=st',state=s',queue=q}
        where (q,st',s') = applyStateFunction s st

\end{code}

To get a few tokens in a row, we can call \code{gettokentil}. The matching
token is left in the queue.

\begin{code}
gettokentil st cond
    | emptyTokenStream st = ([], st)
    | (cond c) = ([],st)
    | otherwise = let (c',st'') = gettokentil st' cond in (c:c', st'')
    where (c,st') = gettoken st

\end{code}

An often needed operation is to skip an optional space or additional equal
signs. We first implement a generic \code{maybetok} function, which takes a
condition and skips a token if it matches that condition.
\begin{code}
maybetok :: (Token -> Bool) -> TokenStream -> TokenStream
maybetok cond st
    | emptyTokenStream st = st
    | otherwise = if cond t then rest else st
    where
        (t,rest) = gettoken st
\end{code}

Now, \code{maybespace} and \code{maybeeq} are simply defined as calls to
\code{maybetok}:
\begin{code}
maybespace = maybetok (\t ->
                case t of
                    (CharToken c) -> category c == Space
                    _ -> False
                    )
maybeeq = maybetok (== (CharToken (TypedChar '=' Other)))
\end{code}

We define a few helper functions to manipulate the stream.

\begin{code}
droptoken = snd . gettoken
\end{code}

We can add tokens to the start of the queue, either one (\code{streampush}) or
several (\code{streamenqueue}).

\begin{code}
streampush st@TokenStream{queue=ts} t = st{queue=(t:ts)}
streamenqueue st@TokenStream{queue=ts} nts = st{queue=(nts ++ ts)}
\end{code}

Sometimes we want to have a simple stream that only spits out the tokens we
initialise it with. This is achieved by \code{tokenliststream}:

\begin{code}
tokenliststream = streamenqueue (newTokenStream $ TypedCharStream [] [])
\end{code}

\code{emptyTokenStream} is surprisingly tricky. In fact, we need to look ahead
to see if there is anything left to read. For example if the input consists of
a few lines of comments, there are still many chars left, but the token stream
will be empty. If the state functions were defined slightly differently (to
always read as much as they could), this function could be simpler, but the
state functions would be potentially more complex.

\begin{code}
emptyTokenStream TokenStream{queue=(_:_)} = False
emptyTokenStream TokenStream{charsource=st, state=s, queue=[]}
    | emptyStream st = True
    | otherwise = (null q && emptyStream st')
        where (q,st',_) = applyStateFunction s st
\end{code}

\begin{code}
toksToStr toks = map charof toks
    where
        charof (CharToken (TypedChar c _)) = c
        charof _ = error "hex.Tokens.toksToStr.charof: Unexpected token"
\end{code}


We also add a function to manipulate the underlying character stream.

\begin{code}
updateCharStream t@TokenStream{charsource=s} f = t{charsource=(f s)}
\end{code}

For debugging and testing purposes, we use a simple helper function. The reason
that it is not so simple for real usage is that some tokens might cause the
manipulation of the character stream (for example, by changing the character
code table).

\begin{code}
chars2tokens str = ts
    where
       (ts,_) = gettokentil st $ const False
       st = newTokenStream $ TypedCharStream plaintexenv str
\end{code}

To make code simpler, we define a \code{TokenStream} monad, abbreviated TkS:

\begin{code}
data TkS a = TkS { runTkS :: TokenStream -> (a,TokenStream) }

instance Monad TkS where
    return a = TkS (\tks -> (a,tks))
    x >>= f = TkS (\tks -> let (a,t') = (runTkS x tks); (b,t'') = (runTkS (f a) t') in (b,t''))

instance Functor TkS where
    fmap f x = TkS (\tks -> let (a,t') = (runTkS x tks) in (f a, t'))

emptyTkS = TkS (\tks -> if (emptyTokenStream tks) then (True,tks) else (False,tks))

gettokenM :: TkS Token
gettokenM = TkS gettoken

skiptokenM = (gettokenM >> return ())
peektokenM = TkS (\tks -> let (t,_) = gettoken tks in (t,tks))
maybespaceM = TkS (\tks -> ((), maybespace tks))
maybeeqM = TkS (\tks -> ((),maybeeq tks))
\end{code}
