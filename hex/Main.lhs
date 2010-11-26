For the moment, a simple driver for Char.annotate.

\begin{code}
module Main where

import Chars (annotate, plaintextable)
import Tokens (chars2tokens)

import System.Environment
\end{code}

For the moment, hex uses the \textit{hex subcommand} convention for its command
line interface. In the future, I expect this will change to \textit{hex
--subcommand}.

The implementation is simple: each subcommand is a function which takes the
input file (as a string) and produces an output string. The \var{function}
table maps strings to these functions.

\begin{code}
chars input = concat $ map show $ map (annotate plaintextable) input
tokens input = concat $ map show $ chars2tokens $ map (annotate plaintextable) input

function "chars" = chars
function "tokens" = tokens
\end{code}

Without any error checking, get the subcommand, the filename, and print out the results.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 1)
    putStrLn $ function (args !! 0) $ input
\end{code}

