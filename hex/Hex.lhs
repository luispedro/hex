\section{Hex Driver}

This is the main driver of the programme.

\begin{code}
module Hex where
import System.IO.Error
import System.FilePath.Posix

import Macros
import Tokens (updateCharStream, TokenStream)
import CharStream (prequeue)
import qualified Environment as E
type HexEnvironment = E.Environment String E.HexType
\end{code}

Processing \tex{\\input} is achieved by prequeueing the characters in the input
file.

\begin{code}
prequeueChars :: [Char] -> TokenStream -> TokenStream
prequeueChars q st = updateCharStream st (\s -> prequeue s q)
\end{code}

\begin{code}
readOneOf [n] = readFile n
readOneOf (n:ns) = (readFile n) `catch` (\e -> if isDoesNotExistError e then readOneOf ns else ioError e)
\end{code}

\code{processinputs} processes the special commands in the \code{Command}
stream. In particular, it process \tex{\\input} and \tex{\\message}.

\begin{code}
processinputs :: [Command] -> HexEnvironment -> IO [Command]
processinputs [] e = return []
processinputs ((InternalCommand env' rest (MessageCommand msg)):cs) e = (putStrLn msg) >>= (return $ processinputs cs e)
processinputs ((InternalCommand env rest (InputCommand nfname)):_) e = do {
            nextfile <- readOneOf possiblefiles;
            cs <- processinputs (expand env $ prequeueChars nextfile rest) (addfileenv nextfile e);
            return cs;
        } `catch` printerror
    where
        printerror err =
            if isDoesNotExistError err then do
                putStrLn ("Could not open file: `" ++ nfname ++ "`")
                putStrLn ("\tAttempted to open one of: " ++ (concat $ map (\n -> (n ++ " ")) possiblefiles))
                cs <- processinputs (expand env rest) e
                return cs
            else
                ioError err
        possiblefiles
            | isAbsolute nfname = [nfname]
            | hasExtension nfname = do
                dir <- searchpath
                return (dir </> nfname)
            | otherwise = do
                dir <- searchpath
                e <- ["hex", "tex"]
                return (dir </> (nfname <.> e))
        searchpath = [currentdir, "."]
        currentdir = case E.lookup "currentfile" e of
            Just (E.HexString f) -> fst $ splitFileName f
            Nothing -> ""
        addfileenv = (E.globalinsert "currentfile") . E.HexString

processinputs (c:cs) e = do
    cs' <- (processinputs cs e)
    return $ (c:cs')
\end{code}
