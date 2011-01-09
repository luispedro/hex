\section{Output}
\begin{code}
module Output where

import qualified Data.ByteString.Lazy as B
import Control.Monad.State

import qualified Environment as E
import Boxes
import Fonts
import DVI
import Measures
import Linebreak
\end{code}

This module gets a sequence of boxes and outputs them.

To actually perform any operations, we start with code to put one element, then
one line, down:

\begin{code}

putvbox b = do
    move_down (height b)
    push
    putvboxcontent (boxContents b)
    pop
    move_down (depth b)
    where
        putvboxcontent (TextContent s) = putstr s
        putvboxcontent (Kern d) = move_right d
        putvboxcontent (HBoxList bs) = putvboxcontentmany $ map boxContents bs
        putvboxcontentmany [] = return ()
        putvboxcontentmany (b:bs) = (putvboxcontent b) >> (putvboxcontentmany bs)
\end{code}

Now we can put down a sequence of lines easily:

\begin{code}
putlines [] = return ()
putlines (ln:lns) = (putvbox ln) >> (putlines lns)
\end{code}

\begin{code}
putpages _ [] = return ()
putpages env (p:ps) = (putpage p) >> (putpages env ps)
    where
        Just (E.HexDimen margintop) = E.lookup "margintop" env
        Just (E.HexDimen marginright) = E.lookup "marginright" env
        vboxes (VBoxList vb) = vb
        putpage page = do
            newpage
            push
            move_down margintop
            move_right marginright
            defineFont cmr10
            selectFont 0
            putlines $ vboxes $ boxContents page
            pop
            eop
\end{code}

\begin{code}

outputBoxes :: E.Environment -> [VBox] -> B.ByteString
outputBoxes env pages = stream $ execState (outputBoxes' pages) emptyStream
    where
        outputBoxes' pages = do
            startfile
            putpages env pages
            endfile
\end{code}
