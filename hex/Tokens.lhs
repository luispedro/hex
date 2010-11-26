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

The main function of this module takes a list of \code{TypedChar}s and produces
a list of \code{Token}s.

Almost everything is a CharToken, except single non-letters or words following
an \code{Escape}. A word is a sequence of \code{Letter}s followed by
non-\code{Letter}. If the non-\code{Letter} is a \code{Space}, we \textit{eat}
it. Otherwise, we leave it be.

\begin{code}
chars2tokens :: [TypedChar] -> [Token]
chars2tokens [] = []
chars2tokens (c:cs)
    | (category c) /= Escape = (CharToken c):(chars2tokens cs)
    | otherwise = (ControlSequence name):(chars2tokens rest)
        where
            (name,rest) = breakup cs
            breakup :: [TypedChar] -> (String, [TypedChar])
            breakup [] = ([],[])
            breakup (c:cs)
                | (category c) /= Letter = ([value c],cs)
                | otherwise = breakup' (c:cs)
            breakup' :: [TypedChar] -> (String, [TypedChar])
            breakup' [] = ([],[])
            breakup' (c:cs)
                | (category c) == Space = ([],cs)
                | (category c) /= Letter = ([],(c:cs))
                | otherwise = ((value c):name', rest')
                    where (name', rest') = breakup' cs
\end{code}
