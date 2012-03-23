\section{Output}
\begin{code}
module Output
    ( outputBoxes
    ) where

import qualified Data.ByteString.Lazy as B
import Control.Monad.State

import qualified Environment as E
import Measures (dflip)
import Boxes
import DVI
\end{code}

This module gets a sequence of boxes and outputs them. It is at a slightly
higher level than the commands in the DVI module.

To actually perform any operations, we start with code to put one element down.
Then code that puts a whole line down, an element at a time:

\begin{code}

putvbox b = do
        move_down (height b)
        push
        putvboxcontent (boxContents b)
        pop
        move_down (depth b)
    where
        putvboxcontent (CharContent c f) = putc c f
        putvboxcontent (Kern d) = move_right d
        putvboxcontent (HBoxList bs) = putvboxcontentmany $ map boxContents bs
        putvboxcontent (TransformedContent tr bc) = transform tr >> (putvboxcontent bc) >> untransform tr
        putvboxcontent (DefineFontContent (fontinfo,_)) = void $ defineFont fontinfo
        putvboxcontent _ = error "hex.Output.putvboxcontent: Cannot handle this type"
        putvboxcontentmany [] = return ()
        putvboxcontentmany (x:xs) = (putvboxcontent x) >> (putvboxcontentmany xs)
        transform (RaiseBox d) = move_up d
        untransform (RaiseBox d) = move_up (dflip d)
\end{code}

Now we can put down a sequence of lines easily. This function could have been
called \code{putvboxes}:

\begin{code}
putlines [] = return ()
putlines (ln:lns) = (putvbox ln) >> (putlines lns)
\end{code}

Finally, we put down whole pages. A page is simply a particular kind of v-box.

\begin{code}
putpages _ [] = return ()
putpages env (p:ps) = (putpage p) >> (putpages env ps)
    where
        Just (E.HexDimen margintop) = E.lookup "margintop" env
        Just (E.HexDimen marginright) = E.lookup "marginright" env
        vboxes (VBoxList vb) = vb
        vboxes _ = error "hex.Output.putpages.vboxes: Not a vbox list!"
        putpage page = do
            _ <- newpage
            push
            move_down margintop
            move_right marginright
            putlines $ vboxes $ boxContents page
            pop
            eop
\end{code}

The exported function puts all of the pages down:

\begin{code}
outputBoxes :: E.Environment String E.HexType -> [VBox] -> B.ByteString
outputBoxes env pages = stream $ execState outputBoxes' emptyStream
    where
        outputBoxes' = do
            startfile
            putpages env pages
            endfile
\end{code}
