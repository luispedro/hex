\section{DVI Decode}
\begin{code}
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified System.IO as SysIO
import Data.Char
import Data.Binary (Word32)
import Data.Bits (testBit)

import DVI
\end{code}

In order to parse DVI files, we need to shift bytes up and down, so we start
with some helper functions. After all, a good programmer can write C in any
language.

\begin{code}
(<<) :: Integer -> Integer -> Integer
x << 8 = x * 256
x << 16 = x * 256 * 256
x << 24 = x * 256 * 256 * 256
_ << _ = error "hex.DVIDecode.<<: Only defined for 8,16,24"
\end{code}

Using these, we can parse the DVI format for 1- to 4-byte integers. Currently,
we do not handle negative numbers.

\begin{code}
build1 v0 = " " ++ (show v)
    where
        v = toInteger v0

build2 v0 v1 = " " ++ (show v)
    where
        v = (v0' << 8) + v1'
        v0' = toInteger v0
        v1' = toInteger v1

build3 v0 v1 v2 = " " ++ (show v)
    where
        v = (v0' << 16) + (v1' << 8) + v2'
        v0' = toInteger v0
        v1' = toInteger v1
        v2' = toInteger v2

build4 v0 v1 v2 v3 = " " ++ (show v)
    where
        v = if v0' `testBit` 7 then complement2 uv else uv
        uv = (v0' << 24) + (v1' << 16) + (v2' << 8) + v3'
        v0' = toInteger v0
        v1' = toInteger v1
        v2' = toInteger v2
        v3' = toInteger v3
\end{code}

This is a bit of a dark corner of Haskell: integer type handling. We convert
from \code{Integer} to \code{Word32}, then perform \code{negate} on that type,
which does the right thing at the bit level, except that the value is always
interpreted as an unsigned quantity. So, we convert back to \code{Integer} and
flip the sign, using \code{negate :: Integer -> Integer}:
\begin{code}
complement2 :: Integer -> Integer
complement2 = negate . toInteger . (negate :: Word32 -> Word32) . fromInteger
\end{code}

We define a couple of wrappers for reading numbers from a stream of bytes. This
could have been written using a Monad interface, but this is so simple that it
would maybe be overkill.

\begin{code}
get1 (d0:ds) = (build1 d0, ds)
get1 _ = error "hex.DVIDecode.get1: not enough bytes."
get2 (d0:d1:ds) = (build2 d0 d1, ds)
get2 _ = error "hex.DVIDecode.get2: not enough bytes."
get3 (d0:d1:d2:ds) = (build3 d0 d1 d2, ds)
get3 _ = error "hex.DVIDecode.get3: not enough bytes."
get4 (d0:d1:d2:d3:ds) = (build4 d0 d1 d2 d3, ds)
get4 _ = error "hex.DVIDecode.get4: not enough bytes."
getk = splitAt
\end{code}

In order to print ASCII characters, we have convert bytes to characters in the
obvious way:

\begin{code}
word8sToChars :: [DVIByte] -> String
word8sToChars = map (chr . fromInteger . toInteger)
\end{code}

Finally we come to the central function of the module, \code{decode}. This is a
series of pattern matches for the various DVI commands.

Currently, we only parse the commands that we output, but it is easily
extensible.

\begin{code}
decode :: [DVIByte] -> [String]
decode [] = []
decode (d:ds)
    | d < 128 = (("char " ++ (show d) ++ "(" ++ [chr $ fromInteger $ toInteger d] ++ ")"):decode ds)
decode (138:ds) = ("nop":decode ds)
\end{code}

The following is the idiom that looks very Monadic, but it's also very simple.
\begin{code}
decode (139:ds) = (("bop" ++ c0 ++ c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6 ++ c7 ++ c8 ++ c9 ++ lastbop):decode rest)
    where
        (c0,r0) = get4 ds
        (c1,r1) = get4 r0
        (c2,r2) = get4 r1
        (c3,r3) = get4 r2
        (c4,r4) = get4 r3
        (c5,r5) = get4 r4
        (c6,r6) = get4 r5
        (c7,r7) = get4 r6
        (c8,r8) = get4 r7
        (c9,r9) = get4 r8
        (lastbop,rest) = get4 r9
decode (140:ds) = ("eop":decode ds)
decode (141:ds) = ("push":decode ds)
decode (142:ds) = ("pop":decode ds)
decode (143:d0:ds) = (("right1" ++ (build1 d0)):decode ds)
decode (144:d0:d1:ds) = (("right2" ++ (build2 d0 d1)):decode ds)
decode (145:d0:d1:d2:ds) = (("right3" ++ (build3 d0 d1 d2)):decode ds)
decode (146:d0:d1:d2:d3:ds) = (("right4" ++ (build4 d0 d1 d2 d3)):decode ds)

decode (147:ds) = ("w0":decode ds)
decode (148:d0:ds) = (("w1" ++ (build1 d0)):decode ds)
decode (149:d0:d1:ds) = (("w2" ++ (build2 d0 d1)):decode ds)
decode (150:d0:d1:d2:ds) = (("w3" ++ (build3 d0 d1 d2)):decode ds)
decode (151:d0:d1:d2:d3:ds) = (("w4" ++ (build4 d0 d1 d2 d3)):decode ds)

decode (152:ds) = ("x0":decode ds)
decode (153:d0:ds) = (("x1" ++ (build1 d0)):decode ds)
decode (154:d0:d1:ds) = (("x2" ++ (build2 d0 d1)):decode ds)
decode (155:d0:d1:d2:ds) = (("x3" ++ (build3 d0 d1 d2)):decode ds)
decode (156:d0:d1:d2:d3:ds) = (("x4" ++ (build4 d0 d1 d2 d3)):decode ds)

decode (157:d0:ds) = (("down1 " ++ (build1 d0)):decode ds)
decode (158:d0:d1:ds) = (("down2" ++ (build2 d0 d1)):decode ds)
decode (159:d0:d1:d2:ds) = (("down3" ++ (build3 d0 d1 d2)):decode ds)
decode (160:d0:d1:d2:d3:ds) = (("down4" ++ (build4 d0 d1 d2 d3)):decode ds)

decode (161:ds) = ("y0":decode ds)
decode (162:d0:ds) = (("y1" ++ (build1 d0)):decode ds)
decode (163:d0:d1:ds) = (("y2" ++ (build2 d0 d1)):decode ds)
decode (164:d0:d1:d2:ds) = (("y3" ++ (build3 d0 d1 d2)):decode ds)
decode (165:d0:d1:d2:d3:ds) = (("y4" ++ (build4 d0 d1 d2 d3)):decode ds)

decode (d:ds)
    | d >= 171 && d < (171 + 64) = (("fnt_num" ++ (build1 (d-171))):decode ds)
    -- 171 + 64 = 235

decode (b:r0)
    | b == 243 = fnt_decode get1
    | b == 244 = fnt_decode get2
    | b == 245 = fnt_decode get3
    | b == 246 = fnt_decode get4
    where
        fnt_decode get = (("fnt_def" ++ k ++ c ++ s ++ d ++ a ++ " " ++ (word8sToChars fpath)):decode rest) where
            (k,r1) = get r0
            (c,r2) = get4 r1
            (s,r3) = get4 r2
            (d,r4) = get4 r3
            (a,r5) = get1 r4
            (l,r6) = get1 r5
            (fpath,rest) = getk ((read $ tail a)+(read $ tail l)) r6

decode (247:r0) = (("pre" ++ i ++ num ++ den ++ mag ++ k ++ " " ++ (word8sToChars text)):decode rest)
    where
        (i,r1) = get1 r0
        (num,r2) = get4 r1
        (den,r3) = get4 r2
        (mag,r4) = get4 r3
        (k,r5) = get1 r4
        kval = head r4
        (text,rest) = getk (fromInteger $ toInteger kval) r5

decode (248:r0) = (("post" ++ p ++ num ++ den ++ mag ++ l ++ u ++ s ++ t):decode rest)
    where
        (p,r1) = get4 r0
        (num,r2) = get4 r1
        (den,r3) = get4 r2
        (mag,r4) = get4 r3
        (l,r5) = get4 r4
        (u,r6) = get4 r5
        (s,r7) = get2 r6
        (t,rest) = get2 r7

decode (249:r0) = (("post_post" ++ q ++ i):decode rest)
    where
        (q,r1) = get4 r0
        (i,r2) = get4 r1
        rest = remove223 r2
        remove223 (223:r) = remove223 r
        remove223 r = r

\end{code}

Finally, for any DVI codes that we do not know about, we let the pattern
matching fail, but leave open the option of uncommenting the code below to get
a look at the codes that are failing.

\begin{code}
decode (d:ds) = (("unknown" ++ (build1 d)):decode ds)
\end{code}

A little helper function for unix like file specification, where either
\code{-} or \emph{no file name} can both stand for \code{stdin}:

\begin{code}
inputfile :: [String] -> IO SysIO.Handle
inputfile [] = do
    SysIO.hSetBinaryMode SysIO.stdin True
    return SysIO.stdin
inputfile ["-"] = inputfile []
inputfile [fname] = SysIO.openBinaryFile fname SysIO.ReadMode
inputfile _ = error "hex.DVIDecode.inputfile: Too many files."
\end{code}

The \code{main} function is trivial:

\begin{code}
main = do
    args <- getArgs
    input <- (inputfile args) >>= B.hGetContents
    putStr $ unlines $ decode $ map (fromInteger . toInteger) $ B.unpack input
\end{code}

