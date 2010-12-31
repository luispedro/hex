\section{Strings}

Haskell does not have a very complete system for basic string manipulation. We
remedy that situation by writing some functions ourselves.

\begin{code}
module String where

import List (isPrefixOf)
\end{code}

First function is \code{find pattern str} which maybe returns the position of
where the first instance of \code{pattern} is found in \code{str}. It returns
\code{Nothing}, if it is not found.

\begin{code}
find :: String -> String -> Maybe Int
find p str
    | isPrefixOf p str = Just 0
    | null str = Nothing
    | otherwise = (find p $ tail str) >>= (Just . (+1))
\end{code}
