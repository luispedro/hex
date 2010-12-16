\section{Output}
\begin{code}
module Output where
import qualified Data.ByteString.Lazy as B
import Control.Monad.State

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
putle le
    | (leType le) == "glue" = move_right (leWidth le)
    | otherwise = putstr (leTypeset le)

putline line = do
    push
    putline' line
    pop
    move_down line_height
    where
        putline' [] = (return ())
        putline' (le:les) = (putle le) >> (putline les)
\end{code}

Now we can put down a sequence of lines easily:

\begin{code}
putlines [] = return ()
putlines (ln:lns) = (putline ln) >> (putlines lns)
\end{code}

\begin{code}

outputBoxes :: [[LineElement]] -> B.ByteString
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
