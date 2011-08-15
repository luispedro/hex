\section{Measures}
\begin{code}
module Measures
    ( Scaled
    , scale
    , scaledToRational
    , Dimen(..)
    , zeroDimen
    , dimenFromInches
    , dimenFromPoints
    , dimenFromFloatingPoints
    , dplus
    , dsub
    , dmul
    , dratio
    , dgt
    , dmax
    , inchesToPoints
    , PaperSize(..)
    ) where

import Ratio
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
scaledToRational (Scaled sp) = sp % (round (scalefactor :: Float))
\end{code}

In this section, we deal with dimensions. The basic unit is \emph{scaled
points}, i.e., points times $2^{16}$.

\begin{code}
data Dimen = Dimen
            { nrScaledPoints :: Integer
            } deriving (Eq)

instance Show Dimen where
    show (Dimen n) = (show n) ++ "pt"

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
(Dimen np0) `dratio` (Dimen np1) = np0 % np1
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

Eventually, this module will grow. For now, we define only paper size and hard
code the size of American letter paper:

\begin{code}
data PaperSize = PaperSize 
                { paperWidth :: Dimen
                , paperHeight :: Dimen
                } deriving (Eq)

-- letterPaper = PaperSize (dimenFromInches 8.5) (dimenFromInches 11)
\end{code}

