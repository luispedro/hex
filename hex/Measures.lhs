\section{Measures}
\begin{code}
module Measures where

import Ratio
\end{code}

In this section, we declare code to deal with dimensions.

The basic unit is \emph{scaled points}, i.e., points times $2^{16}$.

\begin{code}
data Dimen = Dimen
            { nrScaledPoints :: Integer
            } deriving (Eq)

instance Show Dimen where
    show (Dimen n) = (show n) ++ "pt"

instance Ord Dimen where
    compare (Dimen np0) (Dimen np1) = compare np0 np1

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
inchesToPoints = round . (*72)
\end{code}

We also add a couple of convenience converters:

\begin{code}
dimenFromPoints = Dimen . (*(2^16))
dimenFromFloatingPoints :: (RealFrac f) => f -> Dimen
dimenFromFloatingPoints = Dimen . round . (*(2^16))
dimenFromInches = dimenFromPoints . inchesToPoints
zeroDimen = Dimen 0
\end{code}

Eventually, this module will grow. For now, we define only paper size:

\begin{code}
data PaperSize = PaperSize 
                { paperWidth :: Dimen
                , paperHeight :: Dimen
                } deriving (Eq)

letterPaper = PaperSize (dimenFromInches 8.5) (dimenFromInches 11)
\end{code}

