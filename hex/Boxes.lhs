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
    show (Box h d w) = printf "B[[h(%s)d(%s)w(%s)]]" (show h) (show d) (show w)
\end{code}

We also define ``glue'' here (as D.~E. Knuth himself points out, this should
have been called ``springs'', but glue stuck):

\begin{code}
data Glue = Glue
            { size :: Dimen
            , shrinkage :: Dimen
            , expandable :: Dimen
            } deriving (Eq)
instance Show Glue where
    show (Glue w s e) = printf "G[[w(%s)s(%s)e(%s)]]" (show w) (show s) (show e)
\end{code}

