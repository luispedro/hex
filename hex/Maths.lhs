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

import qualified Data.DList as DL
import Data.Maybe
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Writer (Writer, runWriter, tell)
\end{code}

We start by defining the basic \code{MList} structure:

\begin{code}
data MList = MAtom { center :: MList, sup :: Maybe MList, sub :: Maybe MList }
        | MChar Char
        | MRel MList
        | MListList [MList]
        deriving (Eq, Show)
\end{code}

Typesetting math.

We use a \code{Writer} monad inside a \code{Reader} monad to encapsulate the
environment and the current font style.
\begin{code}
type Environment = E.Environment String E.HexType
type MathSet a = ReaderT (Environment, E.MathFontStyle) (Writer (DL.DList HElement)) a

runMathSet :: MathSet () -> (Environment, E.MathFontStyle) -> [HElement]
runMathSet mset (e,st) = DL.toList $ snd $ runWriter $ runReaderT mset (e,st)
\end{code}

We will be outputting element-by-element, so we define \code{tell1} as a useful
abbreviation:
\begin{code}
tell1 = tell . DL.singleton
\end{code}

The main function is \code{setM}, which sets an mlist.
\begin{code}
setM :: MList -> MathSet ()
setM (MChar c) = setmchar 0 c
setM (MListList ml) = mapM_ setM ml
setM (MRel mr) = setM mr
setM (MAtom c up down) = do
    setM c
    when (isJust up) (setup $ fromJust up)
    when (isJust down) (setdown $ fromJust down)

setup   = setapp raise F.sup1
setdown = setapp lower F.sub1


setapp :: (Dimen -> HBox -> HBox) -> (F.FontInfo -> Maybe FixWord) -> MList -> MathSet ()
setapp trans prop ml = do
        (e,st) <- ask
        (_,fnt) <- fontE 2
        let elements = runMathSet (setM ml) (e,sc st)
        let boxes = (unel `map` elements :: [Box H])
        let box = mergeBoxes H boxes
        case prop fnt of
            Just fx -> do
                let v = dimenFromFloatingPoints $ fixToFloat fx
                tell1 (EBox $ (trans v) box)
            Nothing -> error "hex.Maths.setapp: font does not have the math extensions"
    where
        sc E.Textfont = E.Scriptfont
        sc E.Scriptfont = E.Scriptscriptfont
        sc E.Scriptscriptfont = E.Scriptscriptfont
        unel (EBox b) = b
        unel _ = error "hex.Maths.setapp.unel: Not a box"

fontE fam = do
    (e,st) <- ask
    let (fidx,(_,fnt)) = E.mathfont e fam st
    return (fidx, fnt)

setmchar fam c = do
    (fidx,fnt) <- fontE fam
    tell1 (charInFont c fidx fnt)
\end{code}

As usual, we hide it all inside a pure interface, which is exported:
\begin{code}
typesetMList e ml = runMathSet (setM ml) (e, E.Textfont)
\end{code}
