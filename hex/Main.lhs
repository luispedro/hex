For the moment, a simple driver for Char.annotate.

\begin{code}
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Maybe

import Chars (annotate, plaintextable)
import qualified Boxes
import Tokens (chars2tokens)
import Macros (expand, plaintexenv)
import LoadPL (loadPL)
import Modes (vMode)
import Measures (dimenFromInches)
import Output (outputBoxes)
import Environment (startenv)
import PageBreak (breakpages)
\end{code}

For the moment, hex uses the \textit{hex subcommand} convention for its command
line interface. In the future, I expect this will change to \textit{hex
--subcommand}.

The implementation is simple: each subcommand is a function which takes the
input file (as a string) and produces an output string. The \var{function}
table maps strings to these functions.

\begin{code}
chars = map (annotate plaintextable)
tokens = chars2tokens . chars
expanded = (expand plaintexenv) . tokens 
breaklines = (vMode startenv) . expanded
dvioutput = (outputBoxes startenv) . (breakpages (dimenFromInches 7)) . breaklines

function :: String -> String -> IO ()
function "chars" = putStrLn . concat . (map show) . chars
function "tokens" = putStrLn . concat . (map show) . tokens
function "expanded" = putStrLn . concat . (map show) . expanded
function "loadPL" = putStrLn . show . loadPL
function "breaklines" = putStrLn . concat . (map (++"\n")) . (map show) . (map Boxes.boxContents) . breaklines
function "dvioutput" = B.putStr . dvioutput

asBox (Boxes.EBox b) = Just b
asBox _ = Nothing
\end{code}

Without any error checking, get the subcommand, the filename, and print out the results.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 1)
    function (args !! 0) $ input
\end{code}

