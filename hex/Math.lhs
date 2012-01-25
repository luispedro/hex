\section{Mathematics}

\begin{code}
module Math
    ( parseMath
    , MList(..)
    ) where

import Control.Applicative
import Control.Monad
import Text.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Prim as Prim

import Chars
import Macros

\end{code}
\begin{code}
data MList = MAtom { center :: MList, sup :: Maybe MList, sub :: Maybe MList }
        | MChar Char
        | MRel MList
        | MListList [MList]
        deriving (Eq, Show)

\end{code}

This is a standard parser, based on Parsec, but running on \code{Command}s:

\begin{code}
type TokenParser a = Parsec [Command] () a
\end{code}

Two helpers, to match commands and to match character categories:

\begin{code}
incCol pos _ _  = incSourceColumn pos $ sourceColumn pos
match c = Prim.tokenPrim show incCol testChar
    where
        testChar t = if t == c then Just t else Nothing
matchcat :: CharCategory -> TokenParser Command
matchcat cat = Prim.tokenPrim show incCol testChar
    where
      testChar t@(CharCommand (TypedChar _ cat')) = if cat == cat' then Just t else Nothing
      testChar _ = Nothing
\end{code}

Now get match a single character as a \code{MChar}:
\begin{code}
singlechar :: TokenParser MList
singlechar = do
    CharCommand (TypedChar c _) <- Prim.tokenPrim show incCol Just
    return $ MChar c
\end{code}

Now we build up on this:
\begin{code}
inbraces = (match PushCommand *> mlist <* match PopCommand)
node :: TokenParser MList
node = singlechar <|> inbraces

mnode :: TokenParser MList
mnode = do
    c <- node
    down <- optionMaybe (matchcat SubScript >> node)
    up <- optionMaybe (matchcat Superscript >> node)
    return $ MAtom c down up

mlist = (many1 mnode >>= return . MListList)
\end{code}

End of math mode:

\begin{code}
eomath :: TokenParser [Command]
eomath = do
    void $ matchcat MathShift
    getInput

delimited :: TokenParser (MList, [Command])
delimited = do
    ml <- mlist
    me <- eomath
    return (ml,me)

parseMath :: [Command] -> (MList,[Command])
parseMath cs = res
    where
        Right res = parse delimited "<internal>" cs
\end{code}
