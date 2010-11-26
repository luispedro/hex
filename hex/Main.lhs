For the moment, a simple driver for Char.annotate.

\begin{code}
module Main where

import Chars (annotate, plaintextable)
import System.Environment
\end{code}

Without any error checking, parse the first filename on the command line and
print it out.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    putStrLn $ concat $ map show $ map (annotate plaintextable) input

\end{code}

