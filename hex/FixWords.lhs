\section{Fix words}
\begin{code}
module FixWords
    ( FixWord(..)
    , fixToFloat
    , fromFloat
    ) where

import Data.Bits
import Data.Ratio
\end{code}

Several internal \TeX operations are performed on fixed point words:

\begin{code}
newtype FixWord = FixWord Integer deriving (Eq)

bitshift = 16
base = (1 `shiftL` bitshift)

fixToFloat :: FixWord -> Float
fixToFloat (FixWord v) = (fromInteger v)/(fromInteger base)

fromFloat :: Float -> FixWord
fromFloat = FixWord . round . (* (2.0**16.0))

instance Ord FixWord where
    (FixWord a) `compare` (FixWord b) = a `compare` b

instance Num FixWord where
    (FixWord a) * (FixWord b) = FixWord ((a * b) `shiftR` bitshift)
    (FixWord a) + (FixWord b) = FixWord (a + b)
    abs (FixWord a) = FixWord (abs a)
    signum (FixWord a) = fromInteger (signum a)
    fromInteger n = FixWord (n `shiftL` bitshift)

instance Fractional FixWord where
    a / b = a * (recip b)
    recip = fromFloat . recip . fixToFloat
    fromRational = fromFloat . fromRational

instance Real FixWord where
    toRational (FixWord f) = f % base

instance RealFrac FixWord where
    properFraction (FixWord f) = (fromInteger $ f `shiftR` bitshift, FixWord (f .&. 0xffff))

instance Read FixWord where
    readsPrec _ s = [(fromFloat . read $ s, "")]

instance Show FixWord where show w = show (fixToFloat w)
\end{code}
