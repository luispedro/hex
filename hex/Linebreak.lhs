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

data LineElement = LineElement
                        { leType :: String
                        , leWidth :: Dimen
                        , leStretch :: Dimen
                        , leShrink :: Dimen
                        , lePenalty :: Integer
                        , leFlag :: Bool
                        , leTypeset :: String
                        } deriving (Eq)

instance Show LineElement where
    show = leTypeset
\end{code}

We now define some basic constants: a space and infinite and minus-infinite
penalties. A space is not actually constant as it depends on the font, but we
do not worry about that for now.

\begin{code}
spaceGlue = LineElement
                { leType="glue"
                , leWidth=(dimenFromPoints 12)
                , leStretch=(dimenFromPoints 6)
                , leShrink=(dimenFromPoints 3)
                , lePenalty=0
                , leFlag=False
                , leTypeset=" " }
penalty p = LineElement
                { leType="glue"
                , leWidth=zeroDimen
                , leStretch=zeroDimen
                , leShrink=zeroDimen
                , lePenalty=p
                , leFlag=False
                , leTypeset="" }
infPenalty = 10000
minfPenalty = (-10000)
\end{code}

We now transform the commands into \code{LineElement}s.

\begin{code}
lineElement :: Command -> LineElement
lineElement (CharCommand c)
    | c == ' ' = spaceGlue
    | otherwise = LineElement
                        { leType="box"
                        , leWidth=(dimenFromPoints 12)
                        , leStretch=zeroDimen
                        , leShrink=zeroDimen
                        , lePenalty=0
                        , leFlag=False
                        , leTypeset=[c] }
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
                            $ map ((breakParagraphIntoLines lineWidth). preprocessParagraph)
                            $ map (map lineElement)
                            $ paragraphs cmds
\end{code}
