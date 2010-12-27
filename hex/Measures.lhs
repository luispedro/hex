\section{Measures}
\begin{code}
module Measures where
\end{code}

In this section, we declare code to deal with dimensions.

For the moment, we will use \textit{points} as our basic measure.

\begin{code}
data Dimen = Dimen
            { nrPoints :: Integer
            } deriving (Eq)

instance Show Dimen where
    show (Dimen n) = (show n) ++ "pt"

instance Ord Dimen where
    compare (Dimen np0) (Dimen np1) = compare np0 np1

(Dimen np0) `dplus` (Dimen np1) = Dimen $ np0 + np1
(Dimen np0) `dmax` (Dimen np1) = Dimen $ max np0 np1
\end{code}

We write conversions from other metrics that are used. For now, we will only
define \textit{inches}:

\begin{code}
inchesToPoints = round . (*72)
dimenFromInches = Dimen . inchesToPoints
\end{code}

We also add a couple of convinience converters, including a more descriptive
synonym for the \code{Dimen} constructor:

\begin{code}
dimenFromPoints = Dimen
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

