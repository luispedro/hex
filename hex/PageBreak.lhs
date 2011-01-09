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
breakpages size (vb:vbs) = (first:breakpages size rest)
    where
        first = mergeBoxes V (vb:first')
        (first',rest) = breakpages' (size `dsub` (theight vb)) vbs
        theight b = (height b) `dplus` (depth b)
        breakpages' _ [] = ([], [])
        breakpages' s (vb:vbs)
            | hfirst `dgt` s = ([], (vb:vbs))
            | otherwise = ((vb:h),t)
            where
                hfirst = theight vb
                (h,t) = breakpages' (s `dsub` hfirst) vbs

\end{code}
