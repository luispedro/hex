\section{Fonts}
\begin{code}
module Fonts
    ( fontName
    , GliphMetric(..)
    , SpaceInfo(..)
    , FontInfo(..)
    , widthHeightDepth
    ) where
import Data.Convertible
import Data.Convertible.Base ()
import Data.Word
import Data.Char
import FixWords
\end{code}

This module contains utilities for font management.

Names are encoded as \code{Word8}s in files, so we need to convert from
strings:

\begin{code}
fontName :: String -> [Word8]
fontName = map convert
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
                        } deriving (Eq)
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
\end{code}

A linear list is a very poor implementation the gliph information and should
eventually be replaced by a real structure:
\begin{code}
data FontInfo = FontInfo
            { gliphInfo :: [GliphMetric]
            , spaceInfo :: SpaceInfo
            , xheight :: FixWord
            , quad :: FixWord
            , extraspace :: FixWord
            , sup1 :: Maybe FixWord
            , sup2 :: Maybe FixWord
            , sup3 :: Maybe FixWord
            , sub1 :: Maybe FixWord
            , sub2 :: Maybe FixWord
            } deriving (Eq)
instance Show FontInfo where
    show FontInfo {gliphInfo=fi} = concatMap ((++"\n") . show) fi

widthHeightDepth FontInfo {gliphInfo=fi} c = widthHeightDepth' fi
    where
        widthHeightDepth' [] = error ("Not found character:" ++ [c] ++ "(" ++ (show $ ord c) ++ ")")
        widthHeightDepth' ((GliphMetric ch w h d _):gms)
            | c == ch = (w,h,d)
            | otherwise = widthHeightDepth' gms
\end{code}
