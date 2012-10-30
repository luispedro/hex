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

bitshift = 16

fixToFloat :: FixWord -> Float
fixToFloat (FixWord v) = (fromInteger v)/(fromInteger (1 `shiftL` bitshift))

fromFloat :: Float -> FixWord
fromFloat = FixWord . round . (* (2.0**16.0))

instance Num FixWord where
    (FixWord a) * (FixWord b) = FixWord ((a * b) `shiftR` bitshift)
    (FixWord a) + (FixWord b) = FixWord (a + b)
    abs (FixWord a) = FixWord (abs a)
    signum (FixWord a) = fromInteger (signum a)
    fromInteger n = FixWord (n `shiftL` bitshift)

instance Read FixWord where
    readsPrec _ s = [(fromFloat . read $ s, "")]

instance Show FixWord where show w = show (fixToFloat w)
\end{code}
