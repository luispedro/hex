\section{Mathematics}

\begin{code}
module Math
    ( mMode
    , MList(..)
    ) where

import Control.Applicative
import Control.Monad
import Text.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Prim as Prim

import Chars
import Macros
import Modes

\end{code}
\begin{code}
data MList = MAtom { center :: MList, sup :: Maybe MList, sub :: Maybe MList }
        | MChar Char
        | MRel MList
        | MListList [MList]
        deriving (Eq, Show)

\end{code}

Now get match a single character as a \code{MChar}:
\begin{code}
singlechar :: Modes MList
singlechar = do
    CharCommand (TypedChar c _) <- charcommand
    return $ MChar c
\end{code}

Now we build up on this:
\begin{code}
inbraces = (match PushCommand *> mlist <* match PopCommand)
node :: Modes MList
node = singlechar <|> inbraces

mnode :: Modes MList
mnode = do
    c <- node
    down <- optionMaybe (matchcat SubScript >> node)
    up <- optionMaybe (matchcat Superscript >> node)
    return $ MAtom c down up

mlist = (many1 mnode >>= return . MListList)
\end{code}

End of math mode:

\begin{code}
eomath :: Modes ()
eomath = do
    void $ matchcat MathShift

mMode :: Modes MList
mMode = mlist <* eomath
\end{code}
