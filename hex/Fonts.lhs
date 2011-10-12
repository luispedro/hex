\section{Fonts}
\begin{code}
module Fonts
    ( fontName
    , GliphMetric(..)
    , SpaceInfo(..)
    , FontInfo(..)
    , widthHeightDepth
    , cmr10
    ) where
import DVI
import Data.Word
import Data.Char
import FixWords
\end{code}

This module contains utilities for font management.

Names are encoded as \code{Word8}s in files, so we need to convert from
strings:

\begin{code}
fontName :: String -> [Word8]
fontName = map (fromInteger . toInteger . ord)
\end{code}

A font is information about gliphs, ligatures, kerning, and some extra bits.
Currently, we do not implement neither ligatures nor kerning.

\begin{code}
data GliphMetric = GliphMetric
                        { character :: Char
                        , width :: FixWord
                        , height :: FixWord
                        , depth :: FixWord
                        , italicCorrection :: FixWord
                        }
instance Show GliphMetric where
    show g = "G[" ++ [character g]
                ++ ", wd: " ++ (show $ width g)
                ++ ", ht: " ++ (show $ height g)
                ++ ", dp: " ++ (show $ depth g)
                ++ ", ic: " ++ (show $ italicCorrection g)
                ++ "]"
\end{code}

In addition to the gliphs, a font has information on the space:

\begin{code}
data SpaceInfo = SpaceInfo
                    { size :: FixWord
                    , stretch :: FixWord
                    , shrink :: FixWord
                    } deriving (Eq, Show)

data FontInfo = FontInfo
            { gliphInfo :: [GliphMetric]
            , spaceInfo :: SpaceInfo
            }
instance Show FontInfo where
    show (FontInfo fi _) = concat $ map ((++"\n") . show) fi

widthHeightDepth (FontInfo fi _) c = widthHeightDepth' fi
    where
        widthHeightDepth' [] = error ("Not found character:" ++ [c] ++ "(" ++ (show $ ord c) ++ ")")
        widthHeightDepth' ((GliphMetric ch w h d _):gms)
            | c == ch = (w,h,d)
            | otherwise = widthHeightDepth' gms
\end{code}

Currently, there is a single font, which is hard coded: \textsc{cmr10}.

\begin{code}
cmr10  = FontDef 11374260171 (FixWord 0x000a0000) (FixWord 0x000a0000) 0 5 $ fontName "cmr10"
\end{code}

