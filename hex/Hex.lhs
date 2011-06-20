\section{Hex Driver}

This is the main driver of the programme.

\begin{code}
module Hex where
import System.IO.Error

import Macros
import Tokens (updateCharStream, TokenStream)
import CharStream (prequeue)
\end{code}

\begin{code}
prequeueChars :: [Char] -> TokenStream -> TokenStream
prequeueChars q st = updateCharStream st (\s -> prequeue s q)
\end{code}

\begin{code}
processinputs :: [Command] -> IO [Command]
processinputs [] = return []
processinputs ((InternalCommand env' rest (MessageCommand msg)):cs) = (putStrLn msg) >>= (return $ processinputs cs)
processinputs ((InternalCommand env rest (InputCommand nfname)):_) = do {
        nextfile <- readFile nfname;
        cs <- processinputs $ expand env $ prequeueChars nextfile rest;
        return cs;
    } `catch` (\e ->
        if isDoesNotExistError e then do
            putStrLn ("Could not open file: `" ++ nfname ++ "`")
            cs <- processinputs $ expand env rest
            return cs
        else
            ioError e
        )

processinputs (c:cs) = do
    cs' <- (processinputs cs)
    return $ (c:cs')
\end{code}
