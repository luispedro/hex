\section{Output}
\begin{code}
module Output where

import qualified Data.ByteString.Lazy as B
import Control.Monad.State

import Boxes
import Fonts
import DVI
import Measures
import Linebreak
\end{code}

This module gets a sequence of boxes and outputs them.

(Actually, currently it outputs a sequence of LineElement lists).

First we define a few important constants (in truth, they should not be
constant, but for now, that'll do):

\begin{code}
margin_top = (dimenFromInches 1)
margin_right = (dimenFromInches 1)
line_height = (dimenFromPoints 36)
\end{code}

To actually perform any operations, we start with code to put one element, then
one line, down:

\begin{code}

putvbox (EBox b) = putvboxcontent (boxContents b)
    where
        putvboxcontent (TextContent s) = putstr s
        putvboxcontent (Kern d) = move_right d
        putvboxcontent (BoxList bs) = putvboxcontentmany bs
        putvboxcontentmany [] = return ()
        putvboxcontentmany (b:bs) = (putvboxcontent b) >> (putvboxcontentmany bs)


putline line = do
    push
    putvbox line
    pop
    move_down line_height
\end{code}

Now we can put down a sequence of lines easily:

\begin{code}
putlines [] = return ()
putlines (ln:lns) = (putline ln) >> (putlines lns)
\end{code}

\begin{code}

outputBoxes :: [VElement] -> B.ByteString
outputBoxes boxes = stream $ execState (outputBoxes' boxes) emptyStream
    where
        outputBoxes' boxes = do
            startfile
            newpage
            push
            move_down margin_top
            move_right margin_right
            defineFont cmr10
            selectFont 0
            putboxes boxes
            pop
            eop
            endfile
        putboxes = putlines
\end{code}
