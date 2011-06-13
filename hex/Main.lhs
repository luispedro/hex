\begin{code}
module Main where

import qualified Data.Map as Map
import System.Environment
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Maybe

import CharStream -- (annotate,TypedCharStream)
import qualified Boxes
import Tokens
import Macros (expand, emptyenv)
import LoadPL (loadPL)
import Modes (vMode)
import Measures (dimenFromInches)
import Output (outputBoxes)
import Environment (loadfont)
import PageBreak (breakpages)

import Defaults (startenv, plaintexenv)
\end{code}

For the moment, hex uses the \textit{hex subcommand} convention for its command
line interface. In the future, I expect this will change to \textit{hex
--subcommand}.

The implementation is simple: each subcommand is a function which takes the
input file (as a string) and produces an output string. The \var{function}
table maps strings to these functions.

\begin{code}
chars = map (annotate plaintexenv)
tokens = chars2tokens
expanded str = expand emptyenv $ newTokenStream $ TypedCharStream plaintexenv str
breaklines env = (vMode env) . expanded
dvioutput fontinfo = (outputBoxes env) . (breakpages (dimenFromInches 7)) . (breaklines env)
    where env = loadfont fontinfo startenv

function :: String -> String -> IO ()
function "chars" = putStrLn . concat . (map show) . chars
function "tokens" = putStrLn . concat . (map show) . tokens
function "expanded" = putStrLn . concat . (map show) . expanded
function "loadPL" = putStrLn . show . loadPL

function "breaklines" = \input -> do
    fontinfo <- readFile "data/cmr10.pl"
    putStrLn $ concat $ (map (++"\n")) $ (map show) $ filter (\b -> case b of Boxes.Kern _ -> False; _ -> True) $ (map Boxes.boxContents) $ breaklines (loadfont fontinfo startenv) input

function "dvioutput" = \input -> do
    fontinfo <- readFile "data/cmr10.pl"
    B.putStr $ dvioutput fontinfo input


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

