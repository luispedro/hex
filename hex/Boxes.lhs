\section{Boxes}
\begin{code}
module Boxes where
import Text.Printf

import Measures
\end{code}

We define the basic box structure and operations on them. A box can contain a
basic string or a sequence of other boxes (recursively).

\begin{code}
data BoxContents = TextContent String | BoxList [BoxContents] deriving (Eq)
typeset (TextContent str) = str
typeset (BoxList bcs) = concat $ map typeset bcs

typesetChar c = TextContent [c]
\end{code}

There are two types of boxes: horizontal (\code{Hbox}es) and vertical
(\code{VBox}es).

\begin{code}

data H = H deriving (Eq)
data V = V deriving (Eq)
class BoxType a where
    codefor :: a -> String
instance BoxType H where
    codefor _ = "H"
instance BoxType V where
    codefor _ = "V"

data (BoxType t) => Box t = Box
            { boxType :: t
            , height :: Dimen
            , depth :: Dimen
            , width :: Dimen
            , boxContents :: BoxContents
            } deriving (Eq)

type HBox = Box H
type VBox = Box V

instance (BoxType b) => Show (Box b) where
    show (Box t h d w _) = printf "%sB[[h(%s)d(%s)w(%s)]]" (codefor t) (show h) (show d) (show w)

charBox c = Box
        { boxType=H
        , height=zeroDimen
        , width=(dimenFromPoints 12)
        , depth=zeroDimen
        , boxContents=typesetChar c
        }
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

