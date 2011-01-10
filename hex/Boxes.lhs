\section{Boxes}
\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module Boxes where
import Text.Printf

import Measures

\end{code}

We define the basic box structure and operations on them. A box can contain a
basic string or a sequence of other boxes (recursively).


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

\end{code}

\begin{code}
data BoxContents = TextContent String
                    | Kern Dimen
                    | VBoxList [VBox]
                    | HBoxList [HBox] deriving (Eq)
instance Show BoxContents where
    show (TextContent str) = str
    show (Kern d) = " "
    show (HBoxList bcs) = concat $ map (show . boxContents) bcs
    show (VBoxList bcs) = concat $ map (show . boxContents) bcs

typesetChar c = TextContent [c]
\end{code}


\begin{code}
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
\end{code}

We also define ``glue'' here (as D.~E. Knuth himself points out, this should
have been called ``springs'', but glue stuck):

\begin{code}
data (BoxType t) => Glue t = Glue
            { glueType :: t
            , size :: Dimen
            , shrinkage :: Dimen
            , expandable :: Dimen
            } deriving (Eq)
instance (BoxType t) => Show (Glue t) where
    show (Glue t w s e) = printf "%sG[[w(%s)s(%s)e(%s)]]" (codefor t) (show w) (show s) (show e)

type HGlue = Glue H
type VGlue = Glue V

spaceGlue = Glue
            { glueType=H
            , size=(dimenFromPoints 12)
            , expandable=(dimenFromPoints 6)
            , shrinkage=(dimenFromPoints 3)
            }
\end{code}

\begin{code}
data (BoxType t) => Penalty t = Penalty
            { penaltyType :: t
            , value :: Integer
            , flag :: Bool
            } deriving (Eq)
instance (BoxType t) => Show (Penalty t) where
    show (Penalty t v f) = printf "%sP[[%s(%s)]]" (codefor t) (show v) (show f)

type HPenalty = Penalty H
type VPenalty = Penalty V
\end{code}

Finally, we can define \code{H} and \code{V} elements as either a box, a glue,
or a penalty:

\begin{code}
data (BoxType t) => Element t =
                        EBox (Box t)
                        | EGlue (Glue t)
                        | EPenalty (Penalty t)

instance (BoxType t) => Show (Element t) where
    show (EBox b) = "E" ++ show b
    show (EGlue g) = "E" ++ show g
    show (EPenalty p) = "E" ++ show p

type HElement = Element H
type VElement = Element V
\end{code}

A final utility function for dealing with boxes: merging them.

\begin{code}
class BoxListable a where
    boxList :: [a] -> BoxContents
instance BoxListable HBox where
    boxList = HBoxList
instance BoxListable VBox where
    boxList = VBoxList

mergeBoxes t bs = Box
            { boxType=t
            , width=(foldr1 dplus $ map width bs)
            , depth=(foldr1 dmax $ map depth bs)
            , height=(foldr1 dmax $ map height bs)
            , boxContents=(boxList bs)
            }
\end{code}
