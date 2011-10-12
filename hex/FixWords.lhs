\section{Fix words}
\begin{code}
module FixWords
    ( FixWord(..)
    , fixToFloat
    , fixWordFrom32
    ) where
\end{code}

\begin{code}
newtype FixWord = FixWord Float deriving (Eq)
fixToFloat (FixWord f) = f
fixWordFrom32 n = FixWord $ ((fromInteger n) :: Float)/1048576.0 -- 1048576 == (1 << 20)

instance Num FixWord where
    (FixWord a) * (FixWord b) = FixWord (a * b)
    (FixWord a) + (FixWord b) = FixWord (a + b)
    abs (FixWord a) = FixWord (abs a)
    signum (FixWord a) = FixWord (signum a)
    fromInteger n = FixWord (fromInteger n)

instance Show FixWord where show (FixWord w) = show w
\end{code}
