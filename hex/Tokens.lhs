Tokens are the next level after annotated characters. A Token is either a
control sequence or an annotated character.

\begin{code}
module Tokens where
import Chars

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

We start with \code{skiptoeol} which is used for comments. Then, the
implementation of the two easy states: S \& N:

\begin{code}
skiptoeol = dropWhile ((/= EOL) . category)

sN [] = []
sN (c:cs)
    | (category c) == EOL = (ControlSequence "par"):(sN cs)
    | (category c) == Space = sN cs
    | otherwise = sM (c:cs)
sS [] = []
sS (c:cs)
    | (category c) == EOL = sN cs
    | (category c) == Space = sS cs
    | otherwise = sM (c:cs)
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
sM [] = []
sM (c:cs)
    | (category c) == EOL = (CharToken (TypedChar ' ' Space)):(sN cs)
    | (category c) == Space = (CharToken c):sS cs
    | (category c) == Comment = sN (skiptoeol cs)
    | (category c) /= Escape = (CharToken c):(sM cs)
    | otherwise = (ControlSequence name):(nextstate rest)
        where
            (name,rest,nextstate) = breakup cs
            breakup :: [TypedChar] -> (String, [TypedChar], [TypedChar] -> [Token])
            breakup [] = ([],[],sN)
            breakup (c:cs)
                | (category c) == Space = ([value c],cs,sS)
                | (category c) /= Letter = ([value c],cs,sM)
                | otherwise = breakup' (c:cs)
            breakup' :: [TypedChar] -> (String, [TypedChar], [TypedChar] -> [Token])
            breakup' [] = ([],[],sS)
            breakup' (c:cs)
                | (category c) /= Letter = ([],(c:cs), sS)
                | otherwise = ((value c):name', rest', state')
                    where (name', rest', state') = breakup' cs
\end{code}
The main function of this module takes a list of \code{TypedChar}s and produces
a list of \code{Token}s.

\begin{code}
chars2tokens :: [TypedChar] -> [Token]
chars2tokens = sN . (filter ((/= Ignored) . category))

\end{code}

