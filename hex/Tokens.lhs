\section{Tokens}
\begin{code}
module Tokens where

import Chars
import CharStream
import qualified Data.Map as Map
import Defaults (plaintexenv)

\end{code}

Tokens are the next level after annotated characters. A Token is either a
control sequence or an annotated character.

\begin{code}
data Token = ControlSequence String | CharToken TypedChar
instance Show Token where
    show (ControlSequence name) = "[\\" ++ name ++ "]"
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

Now, the implementation of the two easy states: S \& N.

\begin{code}
newtype StateFunction = StateFunction ( TypedCharStream -> ([Token], TypedCharStream, StateFunction) )
applyStateFunction (StateFunction s) st = s st

sN = StateFunction sN' where
    sN' st
        | emptyStream st = ([],st,sN)
        | (category c) == EOL = ([ControlSequence "par"], rest, sN)
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

Almost everything is a CharToken, except single non-letters or words following
an \code{Escape}. A word is a sequence of \code{Letter}s followed by
non-\code{Letter}. Therefore, getting a csname is only done in two steps: if it
is single character, just stop there; otherwise, invoke \code{breakup'}.

The rule of what the next state is are a bit convoluted, but they're taken from
the Texbook.

\begin{code}

sM = StateFunction sM' where
    sM' st
        | emptyStream st = ([], st, sM)
        | (category c) == EOL = ([CharToken (TypedChar ' ' Space)], rest, sN)
        | (category c) == Space = ([CharToken c], rest, sS)
        | (category c) == Comment = applyStateFunction sN $ skiptoeol rest
        | (category c) /= Escape = ([CharToken c], rest, sM)
        where (c,rest) = getchar st
    sM' st = ([ControlSequence name], rest', nextstate)
        where
            (c,rest) = getchar st
            (name, rest', nextstate) = breakup rest
            breakup st
                | emptyStream st = ([],st,sN)
                | (category c) == Space = ([value c], rest, sS)
                | (category c) /= Letter = ([value c], rest, sM)
                | otherwise = breakup' [] st
                where (c,rest) = getchar st
            breakup' acc st
                | emptyStream st = (acc,st,sS)
                | (category c) == Space = (acc, rest, sS)
                | (category c) /= Letter = (acc, st, sM)
                | otherwise = breakup' (acc ++ [value c]) rest
                where
                    (c,rest) = getchar st
\end{code}

\begin{code}
data TokenStream = TokenStream
                { charsource :: TypedCharStream
                , state :: StateFunction
                , queue :: [Token]
                }
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

To get a few tokens in a row, we can call \code{gettokentil}

\begin{code}

gettokentil st cond
    | emptyTokenStream st = ([], st)
    | (cond c) = ([],st)
    | otherwise = let (c',st'') = gettokentil st' cond in (c:c', st'')
    where (c,st') = gettoken st

droptoken = snd . gettoken
streampush st@TokenStream{queue=ts} t = st{queue=(t:ts)}
streamenqueue st@TokenStream{queue=ts} nts = st{queue=(nts ++ ts)}
tokenliststream ts = streamenqueue (newTokenStream $ TypedCharStream e []) ts
    where e = Map.empty

emptyTokenStream TokenStream{queue=(t:_)} = False
emptyTokenStream TokenStream{charsource=st, state=s, queue=[]}
    | emptyStream st = True
    | otherwise = (((length q) == 0) && emptyStream st')
        where (q,st',s') = applyStateFunction s st
\end{code}

We also add a function to manipulate the underlying stream:

\begin{code}
updateCharStream t@TokenStream{charsource=s} f = t{charsource=(f s)}
\end{code}

We add a simple helper function:

\begin{code}
chars2tokens str = ts
    where
       (ts,_) = gettokentil st $ const False
       st = newTokenStream $ TypedCharStream plaintexenv str
\end{code}
