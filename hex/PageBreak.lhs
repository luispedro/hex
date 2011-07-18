\section{Page Breaking}
\begin{code}
module PageBreak where

import Measures
import Boxes
\end{code}

This module breaks pages. A page is simply a \code{VBox} that is as large as a page.

The algorithm is simply \emph{best fit}:

\begin{code}
breakpages :: Dimen -> [VBox] -> [VBox]
breakpages _ [] = []
breakpages textheight (vb:vbs) = (first:breakpages textheight rest)
    where
        first = mergeBoxes V (vb:first')
        (first',rest) = breakpages' (textheight `dsub` (theight vb)) vbs
        theight b = (height b) `dplus` (depth b)
        breakpages' _ [] = ([], [])
        breakpages' s (v:vs)
            | hfirst `dgt` s = ([], (v:vs))
            | otherwise = ((v:h),t)
            where
                hfirst = theight v
                (h,t) = breakpages' (s `dsub` hfirst) vs

\end{code}
