\section{Fonts}
\begin{code}
module Fonts where
import DVI
import Data.Word
import Data.Char
\end{code}

This module contains utilities for font management.

Names are encoded as \code{Word8}s in files, so we need to convert from
strings:

\begin{code}
fontName :: String -> [Word8]
fontName = map (fromInteger . toInteger . ord)
\end{code}

Currently, there is a single font, which is hard coded: \textsc{cmr10}.

\begin{code}
cmr10  = FontDef 11374260171 0x000a0000 0x000a0000 0 5 $ fontName "cmr10"
\end{code}

