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



In order to break up lines \emph{only at word boundaries}, we merge words
(sequences of \code{HBox}es separated by \code{Glue} elements) into single
boxes (words). This is independent of what the words actually consist of (e.g.,
they may contain symbols or other non-alphabetic elements).

\begin{code}
concatenatewords [] = []
concatenatewords (le@(B.EGlue g):les) = (le:concatenatewords les)
concatenatewords cs = (first:concatenatewords rest)
    where
        (firstelems,rest) = break (not . isBox) cs
        first = merge $ map getBox firstelems
        getBox (B.EBox b) = b
        merge bs = B.EBox $ B.mergeBoxes B.H bs
        isBox (B.EBox _) = True
        isBox _ = False
\end{code}

Now we come to the main function: \code{breakParagraphIntoLines}. It currently
uses the \emph{first fit} algorithm.

\begin{code}
breakParagraphIntoLines :: Dimen -> [B.HElement] -> [B.VBox]
breakParagraphIntoLines _ [] = []
breakParagraphIntoLines lineWidth les = (B.mergeBoxes B.V $ toBoxes first):(breakParagraphIntoLines lineWidth rest)
    where
        (first,rest) = splitAt (firstLine zeroDimen les) les
        firstLine _ [] = 0
        firstLine (Dimen 0) (le:les) = 1 + (firstLine (leWidth le) les)
        firstLine n (le:les)
            | n' > lineWidth = 0
            | otherwise = 1 + (firstLine n' les)
            where n' = (n `dplus` (leWidth le))
        toBoxes = catMaybes . (map toBox)
        toBox (B.EBox b) = Just b
        toBox (B.EGlue g) = Just spaceBox
        toBox _ = Nothing
\end{code}

The interface function is \code{breakintolines}:

\begin{code}
breakintolines :: Dimen -> [B.HElement] -> [B.VBox]
breakintolines lw ls = breakParagraphIntoLines lw $ concatenatewords ls
\end{code}

