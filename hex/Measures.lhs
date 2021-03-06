\section{Measures}
\begin{code}
module Measures
    ( Scaled
    , scale
    , scaledToRational
    , Dimen(..)
    , UDimen(..)
    , Glue(..)
    , UGlue(..)
    , Unit(..)
    , asDimen
    , asUDimen
    , zeroDimen
    , zeroUDimen
    , dimenFromInches
    , dimenFromPoints
    , dimenFromFloatingPoints
    , dplus
    , dsub
    , dmul
    , dscalef
    , dratio
    , dgt
    , dflip
    , dmax
    , dimenFromUnit
    , inchesToPoints
    , PaperSize(..)
    ) where

import Data.Ratio

import FixWords

import GHC.Float (float2Double)
\end{code}

Many operations in \TeX{} are performed with fixed point numbers, so we define
them here:

\begin{code}
newtype Scaled = Scaled Integer
scalefactor :: (RealFrac f) => f
scalefactor = fromInteger (2::Integer)^(16::Integer)
scale x = Scaled $ round $ x * scalefactor
\end{code}

And we add a few conversion functions:
\begin{code}
scaledToRational :: Scaled -> Rational
scaledToRational (Scaled sp) = sp % round (scalefactor :: Float)
\end{code}

In this section, we deal with dimensions. The basic unit is \emph{scaled
points}, i.e., points times $2^{16}$.

\begin{code}
data Dimen = Dimen
            { nrScaledPoints :: !Integer
            } deriving (Eq)

instance Show Dimen where
    show (Dimen n) = show n ++ "sp"

instance Ord Dimen where
    compare (Dimen np0) (Dimen np1) = compare np0 np1

\end{code}

For manipulating dimensions, we define a bunch of arithmetic operators, all
with names prefixed with \emph{d}:

\begin{code}
(Dimen np0) `dplus` (Dimen np1) = Dimen $ np0 + np1
(Dimen np0) `dsub` (Dimen np1) = Dimen $ np0 - np1
(Dimen np0) `dgt` (Dimen np1) = np0 > np1
(Dimen np0) `dmax` (Dimen np1) = Dimen $ max np0 np1
dmul :: Dimen -> Rational -> Dimen
(Dimen np0) `dmul` f = Dimen $ round $ (toRational np0) * f
dratio :: Dimen -> Dimen -> Rational

dscalef :: FixWord -> Dimen -> Dimen
f `dscalef` (Dimen p) = Dimen . round $ (fixToFloat f * fromInteger p)

(Dimen np0) `dratio` (Dimen np1) = np0 % np1
dflip :: Dimen -> Dimen
dflip (Dimen np) = Dimen (-np)
\end{code}

We write conversions from other metrics that are used. For now, we will only
define \textit{inches}:

\begin{code}
inchesToPoints :: Double -> Double
inchesToPoints = (*72.0)
\end{code}

We also add a couple of convenience converters:

\begin{code}
dimenFromPoints = Dimen . round . (*scalefactor)
dimenFromFloatingPoints :: (RealFrac f) => f -> Dimen
dimenFromFloatingPoints = Dimen . round . (*scalefactor)

dimenFromInches = dimenFromPoints . inchesToPoints
zeroDimen = Dimen 0
\end{code}

\tex{\Tex} recognises several units:

\begin{code}
data Unit = UnitEm | UnitEn | UnitPt | UnitPx | UnitIn | UnitMu | UnitFil | UnitFill deriving (Eq, Show)
data UDimen = UDimen !FixWord !Unit deriving (Eq, Show)

zeroUDimen = UDimen 0 UnitPt

dimenFromUnit :: Double -> Unit -> Dimen
dimenFromUnit val UnitPt = dimenFromPoints val
dimenFromUnit val UnitIn = dimenFromInches val
dimenFromUnit val UnitFil = Dimen . round $ ((-1.0) * val)
dimenFromUnit val UnitFill = Dimen . round $ ((-1.0) * val)
dimenFromUnit _ u = error ("dimenFromUnit: not implemented: "++show u)

asDimen (UDimen val u) = dimenFromUnit (float2Double . fixToFloat $ val) u
asUDimen (Dimen val) = UDimen (fromInteger val) UnitPt
\end{code}


We also define ``glue'' here (as D.~E. Knuth himself points out, this should
have been called ``springs'', but the word glue stuck).

\begin{code}
data Glue = Glue
            { size :: !Dimen
            , shrinkage :: !Dimen
            , expandable :: !Dimen
            , infLevel :: !Int
            } deriving (Eq, Show)
data UGlue = UGlue
            { usize :: !UDimen
            , ushrinkage :: !UDimen
            , uexpandable :: !UDimen
            , uinfLevel :: !Int
            } deriving (Eq, Show)
\end{code}
Eventually, this module will grow. For now, we define only paper size and hard
code the size of American letter paper:

\begin{code}
data PaperSize = PaperSize 
                { paperWidth :: Dimen
                , paperHeight :: Dimen
                } deriving (Eq)

-- letterPaper = PaperSize (dimenFromInches 8.5) (dimenFromInches 11)
\end{code}

