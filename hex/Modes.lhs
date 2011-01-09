\section{Modes}
\begin{code}
module Modes where

import qualified Environment as E

import Tokens
import Macros
import Measures
import Boxes
import Linebreak
\end{code}

Some commands are v-mode commands, other h-mode commands. We begin with a
series of definitions to distinguish the two.

\begin{code}
isVCommand "vspace" = True
isVCommand _ = False
isHCommand = not . isVCommand
\end{code}

\begin{code}
vMode env [] = []
vMode env cs@((PrimitiveCommand seq):_)
    | isVCommand seq = vMode1 env cs
vMode env cs = hMode env cs
\end{code}

\begin{code}
vMode1 env ((PrimitiveCommand "vthree"):cs) = vMode env cs
\end{code}

We begin by breaking a sequence of commands into paragraphs. \code{paragraph}
gets a single paragraph.

\begin{code}
paragraph :: [Command] -> ([HElement],[Command])
paragraph [] = ([],[])
paragraph cs = (par',rest')
    where
        (par, rest) = break isParagraphBreak cs
        par' = hMode' par
        rest' = case rest of
                    ((PrimitiveCommand "par"):rs) -> rs
                    _ -> rest
        isParagraphBreak (PrimitiveCommand "par") = True
        isParagraphBreak (PrimitiveCommand seq) = isVCommand seq
        isParagraphBreak _ = False
\end{code}

\code{hMode'} is \emph{restricted horizontal mode}: it produces a stream of
HBoxes.

\begin{code}
hMode' :: [Command] -> [HElement]
hMode' = concatenatewords . map toHElement
    where
        toHElement (CharCommand ' ') = EGlue spaceGlue
        toHElement (CharCommand c) = EBox $ charBox c
\end{code}
In order to break up lines \emph{only at word boundaries}, we merge words
(sequences of \code{HBox}es separated by \code{Glue} elements) into single
boxes (words). This is independent of what the words actually consist of (e.g.,
they may contain symbols or other non-alphabetic elements).

\begin{code}
concatenatewords [] = []
concatenatewords (le@(EGlue g):les) = (le:concatenatewords les)
concatenatewords cs = (first:concatenatewords rest)
    where
        (firstelems,rest) = break (not . isBox) cs
        first = merge $ map getBox firstelems
        getBox (EBox b) = b
        merge bs = EBox $ Box
                        { boxType=H
                        , width=(foldr1 dplus $ map width bs)
                        , depth=(foldr1 dmax $ map depth bs)
                        , height=(foldr1 dmax $ map height bs)
                        , boxContents=(BoxList $ map boxContents bs)
                        }
        isBox (EBox _) = True
        isBox _ = False
\end{code}

\end{code}

\begin{code}
hMode :: E.Environment -> [Command] -> [VElement]
hMode env [] = []
hMode env cs = (breakParagraphIntoLines linewidth firstParagraph) ++ (vMode env rest)
    where
        (firstParagraph, rest) = paragraph cs
        Just (E.HexDimen linewidth) = E.lookup "textwidth" env
\end{code}

Finally, we put it all together: input is a sequence of commands and output is
a sequence of lines.
