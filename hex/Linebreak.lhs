\section{Line Breaking}
\begin{code}
module Linebreak where

import Macros
import qualified Boxes as B
import Measures
import Tokens
import Chars
\end{code}

Line breaking is one of TeX's traditional strengths.

We begin by breaking a sequence of commands into paragraphs:

\begin{code}
paragraphs :: [Command] -> [[Command]]
paragraphs [] = []
paragraphs cs = (par:paragraphs rest')
    where
        (par, rest) = break isParagraph cs
        rest' = if length rest > 0 then tail rest else rest
        isParagraph (PrimitiveCommand "par") = True
        isParagraph _ = False
\end{code}

For line breaking, we need to first transform all of the elements into line
elements. We are following the chapter ``Breaking Paragraphs Into Lines'' in
Digital Typography by D.~E. Knuth.

\begin{code}

data LineElement = HBox B.HBox | Glue B.Glue | Penalty Integer Bool deriving (Eq)

leWidth (HBox hb) = B.width hb
leWidth (Glue g) = B.size g
leWidth (Penalty _ _) = zeroDimen
leStretch (HBox hb) = zeroDimen
leStretch (Glue g) = B.expandable g
leStretch (Penalty _ _) = zeroDimen
leShrink (HBox hb) = zeroDimen
leShrink (Glue g) = B.shrinkage g
leShrink (Penalty _ _) = zeroDimen
lePenalty (HBox _) = 0
lePenalty (Glue _) = 0
lePenalty (Penalty p _) = p
leFlag (HBox _) = False
leFlag (Glue _) = False
leFlag (Penalty _ f) = f
leTypeset (HBox hb) = B.typeset $ B.boxContents hb
leTypeset (Glue g) = " "
leTypeset (Penalty _ _) = ""

instance Show LineElement where
    show = leTypeset
\end{code}

We now define some basic constants: a space and infinite and minus-infinite
penalties. A space is not actually constant as it depends on the font, but we
do not worry about that for now.

\begin{code}
spaceGlue = Glue $ B.Glue
                { B.size=(dimenFromPoints 12)
                , B.expandable=(dimenFromPoints 6)
                , B.shrinkage=(dimenFromPoints 3)
                }
penalty p = Penalty p False
infPenalty = 10000
minfPenalty = (-10000)
\end{code}

We now transform the commands into \code{LineElement}s.

\begin{code}
lineElement :: Command -> LineElement
lineElement (CharCommand c)
    | c == ' ' = spaceGlue
    | otherwise = HBox $ B.charBox c
\end{code}

In order to break up lines \emph{only at word boundaries}, we merge words
(sequences of \code{HBox}es separated by \code{Glue} elements) into single
boxes (words). This is independent of what the words actually consist of (e.g.,
they may contain symbols or other non-alphabetic elements).

\begin{code}
concatenatewords :: [LineElement] -> [LineElement]
concatenatewords [] = []
concatenatewords (le@(Glue g):les) = (le:concatenatewords les)
concatenatewords cs = (first:concatenatewords rest)
    where
        (firstelems,rest) = break (not . isBox) cs
        first = merge $ map getBox firstelems
        getBox (HBox b) = b
        merge bs = HBox $ B.HBox
                        { B.width=(foldr1 dplus $ map B.width bs)
                        , B.depth=(foldr1 dmax $ map B.depth bs)
                        , B.height=(foldr1 dmax $ map B.height bs)
                        , B.boxContents=(B.BoxList $ map B.boxContents bs)
                        }
        isBox (HBox _) = True
        isBox _ = False
\end{code}


In order to make everything work out, we need to add special markers to the end
of the paragraph. Currently, they are not used, but, when the full algorithm is
implemented, they will guarantee that the last line of a paragraph is correctly
broken and filled out.

\begin{code}
preprocessParagraph pars = pars ++ [penalty infPenalty, spaceGlue, penalty minfPenalty]
\end{code}

Now we come to the main function: \code{breakParagraphIntoLines}. It currently
uses the \emph{first fit} algorithm.

\begin{code}
breakParagraphIntoLines :: Dimen -> [LineElement] -> [[LineElement]]
breakParagraphIntoLines _ [] = []
breakParagraphIntoLines lineWidth les = (first:breakParagraphIntoLines lineWidth rest)
    where
        (first,rest) = splitAt (firstLine zeroDimen les) les
        firstLine _ [] = 0
        firstLine (Dimen 0) (le:les) = 1 + (firstLine (leWidth le) les)
        firstLine n (le:les)
            | n' > lineWidth = 0
            | otherwise = 1 + (firstLine n' les)
            where n' = (n `dplus` (leWidth le))
\end{code}

Finally, we put it all together: input is a sequence of commands and output is
a sequence of lines:

\begin{code}
commandsToLines :: Dimen -> [Command] -> [[LineElement]]
commandsToLines lineWidth cmds = concat
                            $ map ((breakParagraphIntoLines lineWidth). preprocessParagraph . concatenatewords)
                            $ map (map lineElement)
                            $ paragraphs cmds
\end{code}
