\section{Line Breaking}
\begin{code}
module Linebreak where

import Data.Maybe

import Macros
import qualified Boxes as B
import Measures
import Tokens
import Chars

\end{code}

Line breaking is one of TeX's traditional strengths.

For line breaking, we need to first transform all of the elements into line
elements. We are following the chapter ``Breaking Paragraphs Into Lines'' in
Digital Typography by D.~E. Knuth.

\begin{code}

leWidth (B.EBox hb) = B.width hb
leWidth (B.EGlue g) = B.size g
leWidth (B.EPenalty p) = zeroDimen
leStretch (B.EBox hb) = zeroDimen
leStretch (B.EGlue g) = B.expandable g
leStretch (B.EPenalty p) = zeroDimen
leShrink (B.EBox hb) = zeroDimen
leShrink (B.EGlue g) = B.shrinkage g
leShrink (B.EPenalty p) = zeroDimen
lePenalty (B.EBox _) = 0
lePenalty (B.EGlue _) = 0
lePenalty (B.EPenalty p) = B.value p
leFlag (B.EBox _) = False
leFlag (B.EGlue _) = False
leFlag (B.EPenalty p) = B.flag p
leTypeset (B.EBox hb) = B.typeset $ B.boxContents hb
leTypeset (B.EGlue g) = " "
leTypeset (B.EPenalty p) = ""
\end{code}

We now define some basic constants: a space and infinite and minus-infinite
penalties. A space is not actually constant as it depends on the font, but we
do not worry about that for now.

\begin{code}
spaceEGlue = B.EGlue $ B.spaceGlue
spaceBox = B.Box
                { B.boxType=B.H
                , B.width=(dimenFromPoints 12)
                , B.depth=(dimenFromPoints 0)
                , B.height=(dimenFromPoints 0)
                , B.boxContents=(B.Kern $ dimenFromPoints 12)
                }
penalty p = B.EPenalty $ B.Penalty B.H p False
infPenalty = 10000
minfPenalty = (-10000)
\end{code}

In order to make everything work out, we need to add special markers to the end
of the paragraph. Currently, they are not used, but, when the full algorithm is
implemented, they will guarantee that the last line of a paragraph is correctly
broken and filled out.

\begin{code}
preprocessParagraph pars = pars ++ 
                                [ penalty infPenalty
                                , spaceEGlue
                                , penalty minfPenalty]
\end{code}

\begin{code}
vElementFromHList :: [B.HBox] -> B.VElement
vElementFromHList ves = B.EBox $ B.Box
                { B.boxType=B.V
                , B.width=(foldr1 dplus $ map B.width ves)
                , B.depth=(foldr1 dmax $ map B.depth ves)
                , B.height=(foldr1 dmax $ map B.height ves)
                , B.boxContents=(B.BoxList $ map B.boxContents ves)
                }

toBoxes = catMaybes . (map toBox)
    where
        toBox (B.EBox b) = Just b
        toBox (B.EGlue g) = Just spaceBox
        toBox _ = Nothing
\end{code}

Now we come to the main function: \code{breakParagraphIntoLines}. It currently
uses the \emph{first fit} algorithm.

\begin{code}
breakParagraphIntoLines :: Dimen -> [B.HElement] -> [B.VElement]
breakParagraphIntoLines _ [] = []
breakParagraphIntoLines lineWidth les = (vElementFromHList $ toBoxes first):(breakParagraphIntoLines lineWidth rest)
    where
        (first,rest) = splitAt (firstLine zeroDimen les) les
        firstLine _ [] = 0
        firstLine (Dimen 0) (le:les) = 1 + (firstLine (leWidth le) les)
        firstLine n (le:les)
            | n' > lineWidth = 0
            | otherwise = 1 + (firstLine n' les)
            where n' = (n `dplus` (leWidth le))
\end{code}

