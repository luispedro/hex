For the moment, a simple driver for Char.annotate.

\begin{code}
module Main where

import System.Environment

import Chars (annotate, plaintextable)
import Tokens (chars2tokens)
import Macros (expand, plaintexenv)
import LoadPL (loadPL)
import Linebreak (commandsToLines)
import Measures (dimenFromInches)
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
breaklines = (commandsToLines (dimenFromInches 6)) . expanded

function :: String -> String -> String
function "chars" = concat . (map show) . chars
function "tokens" = concat . (map show) . tokens
function "expanded" = concat . (map show) . expanded
function "loadPL" = concat . map (++"\n") . (map show) . loadPL
function "breaklines" = concat . concat . map (++["\n"]) . (map (map show)) . breaklines
\end{code}

Without any error checking, get the subcommand, the filename, and print out the results.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 1)
    putStrLn $ function (args !! 0) $ input
\end{code}

