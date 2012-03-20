\section{Boxes}

We need to declare a special GHC extension here for use below

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
\end{code}

Now, we can start:

\begin{code}
module Boxes
    ( H(..)
    , V(..)
    , BoxContents(..)
    , Box(..)
    , HBox
    , VBox
    , Glue(..)
    , HGlue
    , VGlue
    , Penalty(..)
    , HPenalty
    , VPenalty
    , Element(..)
    , HElement
    , VElement
    , esize
    , mergeBoxes
    , hboxto
    , spaceInFont
    , charInFont
    ) where
import Text.Printf

import Measures
import qualified Fonts as F
import DVI (FontDef)
import FixWords (fixToFloat)
\end{code}

We define the basic box structure and operations on them. A box can contain a
basic string or a sequence of other boxes (recursively).

There are two types of boxes: horizontal (\code{Hbox}es) and vertical
(\code{VBox}es). The code here is potentially too clever. Its general structure
is that the box types are parameterised by other types (of class
\code{BoxType}). There are two box types: \code{H} and \code{V} for h- and
v-boxes, respectively.

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

For the moment, a box can contain only a few types of things: text (as a
string), a kern (which represents a vspace for vboxes), or a list of other
boxes.

\begin{code}
data BoxContents = CharContent Char Integer -- Char Font
                    | Kern Dimen
                    | VBoxList [VBox]
                    | HBoxList [HBox]
                    | DefineFontContent (FontDef, F.FontInfo)
                    deriving (Eq)
instance Show BoxContents where
    show (CharContent c _) = [c]
    show (Kern _) = " "
    show (HBoxList bcs) = concatMap (show . boxContents) bcs
    show (VBoxList bcs) = concatMap (show . boxContents) bcs
    show (DefineFontContent _) = "{font}"
\end{code}

Finally, we define a box

\begin{code}
data (BoxType t) => Box t = Box
            { boxType :: t
            , height :: Dimen
            , depth :: Dimen
            , width :: Dimen
            , boxContents :: BoxContents
            } deriving (Eq)
\end{code}

For convenience, we have two synonyms for h- and v-boxes.
\begin{code}
type HBox = Box H
type VBox = Box V

instance (BoxType b) => Show (Box b) where
    show (Box t h d w _) = printf "%sB[[w(%s)d(%s)h(%s)]]" (codefor t) (show w) (show d) (show h)
\end{code}

We also define ``glue'' here (as D.~E. Knuth himself points out, this should
have been called ``springs'', but the word glue stuck). As above, we can have
h- and v-glue.

\begin{code}
data (BoxType t) => Glue t = Glue
            { glueType :: t
            , size :: Dimen
            , shrinkage :: Dimen
            , expandable :: Dimen
            , infLevel :: Integer
            } deriving (Eq)
instance (BoxType t) => Show (Glue t) where
    show (Glue t w s e i) = printf "%sG[[w(%s)s(%s)e(%s)i(%s)]]" (codefor t) (show w) (show s) (show e) (show i)

type HGlue = Glue H
type VGlue = Glue V
\end{code}

The final type of element we can have are \emph{penalties}, h- or v-penalties:

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
    show (EBox b) = 'E' : show b
    show (EGlue g) = 'E' : show g
    show (EPenalty p) = 'E' : show p

type HElement = Element H
type VElement = Element V
\end{code}

Elements have a natural size

\begin{code}
esize :: HElement -> Dimen
esize (EGlue g) = size g
esize (EBox b) = width b
esize _ = zeroDimen
\end{code}

A simple utility function for dealing with boxes: merging them. This is the
piece of code that requires the \code{TypeSynonymInstances} declared above.
The usage of a class is only a way to get a polymorphic \code{boxList}
function.

\begin{code}
class BoxListable a where
    boxList :: [a] -> BoxContents
instance BoxListable HBox where
    boxList = HBoxList
instance BoxListable VBox where
    boxList = VBoxList

mergeBoxes t bs = Box
            { boxType=t
            , width=(foldr (dplus .width) zeroDimen bs)
            , depth=(foldr (dmax . depth) zeroDimen bs)
            , height=(foldr (dmax . height) zeroDimen bs)
            , boxContents=(boxList bs)
            }
\end{code}

The \code{hboxto} is the equivalent of the \TeX{} level \tex{hbox to}
construct, it stretches or shrinks the glues to make the whole list of elements
as close as possible to the given dimensions.

\begin{code}
hboxto :: Dimen -> [HElement] -> [HElement]
hboxto target es = converted
    where
        naturalsize = (foldr1 dplus $ map esize es)
        update g f = g{size=(size g) `dop` ( (operation g) `dmul` f)}
        (diffsize, operation, dop) = if naturalsize `dgt` target
                                    then (naturalsize `dsub` target, shrinkage, dsub)
                                    else (target `dsub` naturalsize, expandable, dplus)
        total = (foldr1 dplus $ map (\e -> case e of EGlue g -> operation g; _ -> zeroDimen) es)
        factor = if diffsize `dgt` total then 1.0 else diffsize `dratio` total
        converted = map transform es
        transform (EGlue g) = EGlue $ update g factor
        transform e = e
\end{code}

We add a few helpers to build basic glues \&{} boxes:
\begin{code}
spaceInFont fnt = EGlue $ Glue H (f2d spS) (f2d spSt) (f2d spShr) 0
    where (F.SpaceInfo spS spSt spShr) = F.spaceInfo fnt

charInFont c fidx fnt = EBox $ Box
                    { boxType=H
                    , width=(f2d w)
                    , height=(f2d h)
                    , depth=(f2d d)
                    , boxContents=(CharContent c fidx)
                    } where (w,h,d) = F.widthHeightDepth fnt c
f2d = dimenFromFloatingPoints . fixToFloat
\end{code}
