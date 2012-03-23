\begin{code}
module ParseTFM
    ( parseTFM
    , _breakByte
    ) where
\end{code}
\section{Parse TFM Files}

This module parses TFM files. It is based on
\url{http://www-users.math.umd.edu/~asnowden/comp-cont/tfm.html}.

\begin{code}
import qualified Data.ByteString.Lazy as B
import Data.Convertible
import Data.Convertible.Base ()
import Data.Binary.Get
import Data.Bits
import Data.Char
import Control.Monad
import Control.Exception

import FixWords
import Fonts
import DVI
\end{code}

The format is big-endian, but we hide these details behind a few simple
one-liners. We will use the \haskell{Data.Binary.Get} monad.
\begin{code}
getWord16 = getWord16be
getWord = getWord32be
getByte = getWord8
getSigned32 = from32bit `fmap` getWord

from32bit v
    | v > 0x7fffffff = (convert v) - 0x100000000
    | otherwise = convert v

getFixWord = (FixWord . (`shiftR` 4)) `fmap` getSigned32
\end{code}

There several fix word arrays, so we define a little helper function to get
them:

\begin{code}
getFixWordArray n = (const getFixWord) `mapM` [1..n]
\end{code}

The TFM file uses a palette-like scheme for the sizes of characters. They are
not given directly. Instead, they are given as indices into several tables.

The \haskell{CharInfo} type just keeps the pallette indices in width, height,
depth, italic order (which is the same order as they will appear in the file).

\begin{code}
data CharInfo = CharInfo Int Int Int Int deriving (Show, Eq)
\end{code}

Most of the work is done by this monadic function.

\begin{code}
parseTFMM fname = do
    _lf <- getWord16 --  1
    lh <- getWord16 --  2
    bc <- getWord16 --  3
    ec <- getWord16 --  4
    nw <- getWord16 --  5
    nh <- getWord16 --  6
    nd <- getWord16 --  7
    ni <- getWord16 --  8
    nl <- getWord16 --  9
    nk <- getWord16 -- 10
    ne <- getWord16 -- 11
    np <- getWord16 -- 12
    (checksum, dsize, _coding, _family) <- parseHeader lh
    ci <- parseCharInfo bc ec
    widths <- getFixWordArray nw
    heights <- getFixWordArray nh
    depths <- getFixWordArray nd
    italics <- getFixWordArray ni
    when ((fixToFloat $ head widths) /= 0.0) (fail "hex.ParseTFM: widths[0] should be zero")
    when ((fixToFloat $ head heights) /= 0.0) (fail "hex.ParseTFM: widths[0] should be zero")
    when ((fixToFloat $ head depths) /= 0.0) (fail "hex.ParseTFM: widths[0] should be zero")
    when ((fixToFloat $ head italics) /= 0.0) (fail "hex.ParseTFM: widths[0] should be zero")
    _ligkern <- getFixWordArray nl
    _kern <- getFixWordArray nk
    _ext <- getFixWordArray ne
    parameters <- getFixWordArray np
    e <- isEmpty
    unless e (fail "hex.ParseTFM: EOF expected")
    return  ( FontDef (convert checksum) dsize dsize 0 (convert $ length fname) $ fontName fname
            , fromParameters dsize ci widths heights depths italics parameters
            )

fromParameters dsize ci widths heights depths italics parameters = FontInfo gmetrics spinfo xh q esp s1 s2 s3 sb1 sb2
    where
        gmetrics = gliphMetrics dsize ci widths heights depths italics
        spinfo = spaceInfoFromParameters parameters
        xh = parameters !! 4
        q = parameters !! 5
        esp = parameters !! 6
        s1 = try_get 12
        s2 = try_get 13
        s3 = try_get 14
        sb1 = try_get 15
        sb2 = try_get 16
        try_get pos = if length parameters >=  pos then Just (parameters !! pos) else Nothing
        spaceInfoFromParameters (_slant:space:sp_stretch:sp_shrink:_) = SpaceInfo (dsize * space) (dsize * sp_stretch) (dsize * sp_shrink)
        spaceInfoFromParameters _ = error "hex.ParseTFM.spaceInfoFromParameters: Not enough parameters"
\end{code}

Once the main arrays have been extracted, the functions below build the right structures.

\begin{code}

gliphMetrics dsize cis widths heights depths italics = gliph1 `map` (zip cis [0..])
    where
        gliph1 (CharInfo wi hi di ii, c) =
            GliphMetric
                (chr c)
                (dsize * (widths !! (assert (wi > 0) wi)))
                (dsize * (heights !! hi))
                (dsize * (depths !!  di))
                (dsize * (italics !! ii))

parseHeader n = do
    checksum <- getWord32be
    dsize <- getFixWord
    coding <- getBytes 40
    fam <- getBytes 20
    _seven_bit_safe_flag <- getByte
    _ <- getByte
    _ <- getByte
    _wse <- getByte
    _ <- getBytes $ (*4) $ convert (n - 18)
    return (checksum, dsize, coding, fam)

parseCharInfo bc ec = (const parseCharInfo1) `mapM` [bc..ec]
    where 
        parseCharInfo1 = do
            wi <- getByte
            (hi,di) <- (_breakByte 4) `liftM` getByte
            (ii,_tag) <- (_breakByte 6) `liftM` getByte
            _remainder <- getByte
            return $ CharInfo (convert wi) (convert hi) (convert di) (convert ii)
\end{code}

This little helper breaks up a Byte.

\begin{code}
_breakByte n b = (b `shiftR` n, b .&. ((1 `shiftL` n)-1))
\end{code}

Finally, the pure interface function just runs the monadic \haskell{parseTFMM}:

\begin{code}
parseTFM :: String -> B.ByteString -> (FontDef, FontInfo)
parseTFM fname = runGet (parseTFMM fname)
\end{code}

