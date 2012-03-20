\section{Math Typesetting}
\begin{code}
module Maths
    ( MList(..)
    , typesetMList
    ) where

import qualified Environment as E
import Boxes

import Data.Maybe
\end{code}

Now comes math mode:

\begin{code}
data MList = MAtom { center :: MList, sup :: Maybe MList, sub :: Maybe MList }
        | MChar Char
        | MRel MList
        | MListList [MList]
        deriving (Eq, Show)
\end{code}

Typesetting math:
\begin{code}

typesetMList e = set E.Textfont
    where
        set :: E.MathFontStyle -> MList -> [HElement]
        set st (MChar c) = setmchar e c st
        set st (MListList ml)= concat $ (set st) `map` ml
        set st (MRel ml)= set st ml
        set st MAtom { center=c, sup=up, sub=down} = set st c ++ setmaybe (sc st) up ++ setmaybe (sc st) down
        setmaybe _ Nothing = []
        setmaybe st (Just ml) = set st ml
        sc E.Textfont = E.Scriptfont
        sc E.Scriptfont = E.Scriptscriptfont
        sc E.Scriptscriptfont = E.Scriptscriptfont

setmchar e c st = [charInFont c fidx fnt]
    where
        (fidx,(_,fnt)) = E.mathfont e fam st
        fam = 0
\end{code}
