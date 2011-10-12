\section{Fix words}
\begin{code}
module FixWords
    ( FixWord(..)
    , fixToFloat
    ) where

import Data.Bits
\end{code}

Several internal \TeX operations are performed on fixed point words:

\begin{code}
newtype FixWord = FixWord Integer deriving (Eq)

conversionfactor = (1 `shiftL` 20)

fixToFloat :: FixWord -> Float
fixToFloat (FixWord v) = (fromInteger v)/(fromInteger conversionfactor)

instance Num FixWord where
    (FixWord a) * (FixWord b) = FixWord ((a * b) `shiftR` 20)
    (FixWord a) + (FixWord b) = FixWord (a + b)
    abs (FixWord a) = FixWord (abs a)
    signum (FixWord a) = fromInteger (signum a)
    fromInteger n = FixWord (n `shiftL` 20)

instance Show FixWord where show w = show (fixToFloat w)
\end{code}
