\section{Boxes}
\begin{code}
module Boxes where
import Text.Printf

import Measures
\end{code}

We define the basic box structure and operations on them:

\begin{code}
data Box = Box
            { height :: Dimen
            , depth :: Dimen
            , width :: Dimen
            } deriving (Eq)
instance Show Box where
    show (Box h d w) = printf "[[h(%s)d(%s)w(%s)]]" (show h) (show d) (show w)
\end{code}



