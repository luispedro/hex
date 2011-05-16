\section{Defaults}
\begin{code}
module Defaults where

import qualified Data.Map as M

import qualified Environment as E
import Chars
import Measures
\end{code}

The plaintex table needs to be defined in code, at least in part. Some of it
could be defined in HeX code if we have enough to correctly parse \\chardef.

This is a ASCII only implementation (at least in the sense that only the
non-accented 26 English letters are tagged as letters). For fuller unicode
support, this might need to be extended in the future.

\begin{code}
plaintexenv = [M.fromList $ [('\\', Escape)
                         ,('{', BeginGroup)
                         ,('}', EndGroup)
                         ,('$', MathShift)
                         ,('&', AlignmentTab)
                         ,('\n', EOL)
                         ,('#', Parameter)
                         ,('^', Superscript)
                         ,('_', SubScript)
                         ,('\0', Ignored)
                         ,(' ', Space)
                         ,('~', Active)
                         ,('%', Comment)
                         ,('\8', Invalid)
                         ] ++ map (\c -> (c, Letter)) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                ]
\end{code}


To start with, we have an environment with the textwidth register set. In the
future, this should be done in a hex startup file.

DVI already has a $1''$ right and $1''$ top margin by default, so we do not
need to add to this and set our margins to zero. A better system would be to
have the DVI output routines figure themselves out.

\begin{code}
startenv =
        E.globalinsert "textwidth" (E.HexDimen (dimenFromInches 6)) $
        E.globalinsert "margintop" (E.HexDimen zeroDimen) $
        E.globalinsert "marginright" (E.HexDimen zeroDimen) $
        E.empty
\end{code}
