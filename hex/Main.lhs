\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as TS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Console.CmdArgs
import Control.Monad
import Control.Monad.RWS.Strict

import CharStream (asqueue,prequeue,annotate,TypedCharStream(..))
import qualified Boxes
import Tokens
import Macros (expand, gettokentilM)
import Modes (vMode)
import Measures (dimenFromInches)
import Output (outputBoxes)
import PageBreak (breakpages)
import Hex (processcommands, readFont)

import Defaults (startenv, plaintexenv)
import qualified Environment as E
\end{code}

The version is kept right at the top so it is easy to spot and change.

\begin{code}
version = "0.0.6+git"
\end{code}

The implementation is simple: each subcommand is a function which takes the
input file (as a string) and produces an output string. The \haskell{function}
maps strings to these functions.

\begin{code}
prefix = LT.pack "\\hexinternal{loadfont}{cmr10}\\hexinternal{selectfont}{cmr10}"

chars = map (annotate plaintexenv) . LT.unpack
tokens str = fst3 $ runRWS (gettokentilM $ const False) "top" (undefined,astokenstream str)
    where
        fst3 (a,_,_) = a
        astokenstream = newTokenStream . TypedCharStream plaintexenv

expanded = expand . newTokenStream . TypedCharStream plaintexenv
breaklines env = (vMode env) . expanded

function :: String -> LT.Text -> IO ()
function "chars" = putStrLn . concatMap show . chars
function "tokens" = putStrLn . concatMap show . tokens . asqueue "<input>"
function "expanded" = putStrLn . concatMap show . expanded . asqueue "<input>"
function "commands" = \inputstr -> let
            fileq :: TokenStream
            fileq = newTokenStream $ TypedCharStream plaintexenv $ asqueue ("<input>" :: String) inputstr
        in (processcommands (expand fileq) startenv) >>= (putStrLn . concatMap show)
function "breaklines" = \inputstr -> do
    fontinfo <- readFont "cmr10"
    putStrLn $
            concatMap ((++"\n") . show) $
            filter (\b -> case b of Boxes.Kern _ -> False; _ -> True) $
            (map Boxes.boxContents) $
            breaklines (E.setfont 0 fontinfo startenv) (asqueue "<input>" inputstr)
function hmode = \_ -> putStrLn ("Error: unknown mode `" ++ hmode ++ "`")

\end{code}


\haskell{hex} is the main function, taking an input filename and optionally
outputing the resulting DVI.
\begin{code}
hex :: Bool -> String -> IO ()
hex output "-" = hex output "/dev/stdin"
hex output fname = do
        let inputstr :: LT.Text
            inputstr = LT.fromChunks $ map TS.pack ["\\input ", fname, "%\n"]
            fileq :: TokenStream
            fileq = newTokenStream $ TypedCharStream plaintexenv $ asqueue (fname :: String) inputstr
            q = updateCharStream fileq (prequeue ("prexif",prefix))

        commands <- processcommands (expand q) startenv
        let result = buildout startenv commands
        when output (B.putStr result)
        return ()
    where
        buildout env = (outputBoxes env) . (breakpages (dimenFromInches 7)) . (vMode env)
\end{code}

There are currently two options, the mode and a file name. Both are optional,
mode defaults to "hex", and filename to "-" (i.e., standard input).
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
            summary sumtext &=
            details ["Hex implements the TeX language"]
    where sumtext = concat ["Hex v", version, " (C) Luis Pedro Coelho 2011-2013"]
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
            str <- (if f == "-" then LT.getContents else LT.readFile f)
            function m str
\end{code}

