\begin{code}
{-# LANGUAGE DeriveDataTypeable  #-}
module Main where

import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs
import Control.Monad
import System.Process (readProcess)

import CharStream -- (annotate,TypedCharStream)
import qualified Boxes
import Tokens
import Macros (expand)
import Modes (vMode)
import Measures (dimenFromInches)
import Output (outputBoxes)
import PageBreak (breakpages)
import Hex (processinputs)
import ParseTFM
import DVI
import Fonts

import Defaults (startenv, plaintexenv)
import qualified Environment as E
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
expanded str = expand E.empty $ newTokenStream $ TypedCharStream plaintexenv str
breaklines env = (vMode env) . expanded

fontpath :: String -> IO String
fontpath fname = liftM init $ readProcess "kpsewhich" [fname ++ ".tfm"] []
readFont :: String -> IO (FontDef,FontInfo)
readFont fname = do
    absname <- fontpath fname
    fontstr <- B.readFile absname
    return $ parseTFM fname fontstr

function :: String -> String -> IO ()
function "chars" = putStrLn . concat . (map show) . chars
function "tokens" = putStrLn . concat . (map show) . tokens
function "expanded" = putStrLn . concat . (map show) . expanded

function "breaklines" = \inputstr -> do
    fontinfo <- readFont "cmr10"
    putStrLn $
            concat $
            (map (++"\n")) $
            (map show) $
            filter (\b -> case b of Boxes.Kern _ -> False; _ -> True) $
            (map Boxes.boxContents) $
            breaklines (E.loadfont fontinfo startenv) inputstr
function hmode = \_ -> do
    putStrLn ("Error: unknown mode `" ++ hmode ++ "`")

hex output "-" = hex output "/dev/stdin"
hex output fname = do
        fontinfo <- readFont "cmr10"
        inputstr <- readFile fname
        commands <- processinputs (expanded inputstr) startingenv
        env <- return $ E.loadfont fontinfo startingenv
        result <- return $ buildout env commands
        when output (B.putStr result)
        return ()
    where
        startingenv = E.globalinsert "currentfile" (E.HexString fname) startenv
        buildout env = (outputBoxes env) . (breakpages (dimenFromInches 7)) . (vMode env)

asBox (Boxes.EBox b) = Just b
asBox _ = Nothing
\end{code}

There are currently two options, a mode and a file name:

\begin{code}
data HexCmd = HexCmd
                { mode :: String
                , input :: String
                } deriving (Eq, Show, Data, Typeable)
hexcmds = HexCmd
            { mode = "hex" &= help "Hex mode"
            , input = "-" &= argPos 0 &= opt "-"
            } &=
            verbosity &=
            summary "Hex v0.0.3-git (C) Luis Pedro Coelho 2011" &=
            details ["Hex implements the TeX language"]

\end{code}

Main function does not do a lot of error checking, but just gets the arguments
and executes the corresponding command:

\begin{code}
main :: IO ()
main = do
    HexCmd m f <- cmdArgs hexcmds
    case m of
        "hex" -> hex True f
        "hex-silent" -> hex False f
        _ -> do
            str <- (if f == "-" then getContents else readFile f)
            function m str
\end{code}

