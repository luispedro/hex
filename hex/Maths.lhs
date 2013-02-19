\section{Math Typesetting}
\begin{code}
module Maths
    ( MList(..)
    , typesetMList
    ) where

import qualified Environment as E
import qualified Fonts as F
import FixWords (FixWord, fixToFloat)
import Boxes
import Measures

import qualified Data.AList as AL
import Data.Maybe
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Writer (Writer, runWriter, tell)
\end{code}

We start by defining the basic \code{MList} structure:
\begin{code}
data MList = MAtom { center :: MList, sup :: Maybe MList, sub :: Maybe MList }
        | MChar Integer Char
        | MRel MList
        | MBin MList
        | MListList [MList]
        deriving (Eq, Show)
\end{code}
The \code{MRel} and \code{MBin} are about the spacing. There are no semantics
here. You can have a 2 inside and \code{MRel} and \TeX\ will happily typeset it
as relationship, and you can have two relationships next to each other and that
will be just fine too.

We use a \code{Writer} monad inside a \code{Reader} monad to encapsulate the
environment and the current font style.
\begin{code}
type Environment = E.Environment String E.HexType
type MathSet a = ReaderT (Environment, E.MathFontStyle) (Writer (AL.AList HElement)) a

runMathSet :: MathSet () -> (Environment, E.MathFontStyle) -> [HElement]
runMathSet mset (e,st) = AL.toList $ snd $ runWriter $ runReaderT mset (e,st)
\end{code}

We will be outputting element-by-element, so we define \code{tell1} as a useful
abbreviation:
\begin{code}
tell1 = tell . AL.singleton
\end{code}

The main function is \code{setM}, which sets an mlist.
\begin{code}
setM :: MList -> MathSet ()
setM (MChar f c) = setmchar f c
setM (MListList ml) = setmlist ml
setM (MBin mb) = setM mb
setM (MRel mr) = setM mr
setM (MAtom c up down) = do
    setM c
    when (isJust up) (setup $ fromJust up)
    when (isJust down) (setdown $ fromJust down)
\end{code}

Setting a list is a mix of setting elements and inter-element spacing:

\begin{code}
setmlist [] = return ()
setmlist [m] = setM m
setmlist (m0:m1:ms) = do
    setM m0
    spacingM m0 m1
    setmlist (m1:ms)
\end{code}


Typesetting sub/superscripts is accomplished by just moving the boxes up and down.
\begin{code}
setup   = setmove raise F.sup1
setdown = setmove lower F.sub1

setmove :: (Dimen -> HBox -> HBox) -> (F.FontInfo -> Maybe FixWord) -> MList -> MathSet ()
setmove trans prop ml = do
        (e,st) <- ask
        (_,fnt) <- fontE 2
        let elements = runMathSet (setM ml) (e,sc st)
        let boxes = (unel `map` elements :: [Box H])
        let box = mergeBoxes H boxes
        case prop fnt of
            Just fx -> do
                let v = dimenFromFloatingPoints $ fixToFloat fx
                tell1 (EBox $ (trans v) box)
            Nothing -> error "hex.Maths.setmove: font does not have the math extensions"
    where
        sc E.Textfont = E.Scriptfont
        sc E.Scriptfont = E.Scriptscriptfont
        sc E.Scriptscriptfont = E.Scriptscriptfont
        unel (EBox b) = b
        unel _ = error "hex.Maths.setmove.unel: Not a box"

fontE fam = do
    (e,st) <- ask
    let (fidx,(_,fnt)) = E.mathfont e fam st
    return (fidx, fnt)

setmchar fam c = do
    (fidx,fnt) <- fontE fam
    tell1 (charInFont c fidx fnt)
\end{code}

\begin{code}
putSpace :: Integer -> MathSet ()
putSpace 0 = return ()
putSpace _ = do
    (_,fnt) <- fontE 0
    tell1 $ spaceInFont fnt
\end{code}

Spacing is done by a simple look up system
\begin{code}
spacingM a b = do
        (_,sc) <- ask
        let v = spacing (sc /= E.Textfont)
        putSpace v
    where
        spacing in_script = if spvalue < 4 then spvalue
                        else if not in_script then spvalue - 4
                        else 0
--        mtypei MOrd _       = 0
--        mtypei MOp _        = 1
        mtypei (MBin _)       = 2
        mtypei (MChar _ _)    = 0
        mtypei (MAtom c _ _)  = mtypei c
        mtypei (MRel _)       = 3
--        mtypei MOpen  _     = 4
--        mtypei MClose _     = 5
--        mtypei MPunct _     = 6
        mtypei (MListList  _) = 7
        spvalue = table !! (mtypei a) !! (mtypei b)
        table =
         --   Ord      Op   Bin    Rel   Open  Close  Punct  Inner
            [[    0,     1, (2+4), (3+4),     0,     0,     0, (1+4)]  --Ord
            ,[    1,     1,    -1, (3+4),     0,     0,     0, (1+4)]  --Op
            ,[(2+4), (2+4),    -1,    -1, (2+4),    -1,    -1, (2+4)]  --Bin
            ,[(3+4), (3+4),    -1,     0, (3+4),     0,     0, (3+4)]  --Rel
            ,[    0,     0,    -1,     0,     0,     0,     0,     0]  --Open
            ,[    0,     1, (1+4), (3+4),     0,     0,     0, (1+4)]  --Close
            ,[(1+4), (1+4),    -1, (1+4), (1+4), (1+4), (1+4), (1+4)]  --Punct
            ,[(1+4),     1, (1+4), (3+4), (1+4),     0, (1+4), (1+4)]  --Innter
            ]
\end{code}

As usual, we hide it all inside a pure interface, which is exported:
\begin{code}
typesetMList  :: Environment -> MList -> [HElement]
typesetMList e ml = runMathSet (setM ml) (e, E.Textfont)
\end{code}
