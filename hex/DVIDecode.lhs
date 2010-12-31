\section{DVI Decode}
\begin{code}
module Main where

import System.Environment
import qualified Data.ByteString.Lazy as B
import Data.Char

import DVI
\end{code}

In order to parse 

\begin{code}
(<<) :: Integer -> Integer -> Integer
x << 8 = x * 256
x << 16 = x * 256 * 256
x << 24 = x * 256 * 256 * 256
\end{code}

\begin{code}
word8sToChars :: [DVIByte] -> [Char]
word8sToChars = map (chr . fromInteger . toInteger)
\end{code}

\begin{code}
build1 v0 = " " ++ (show $ toInteger v0)
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
        v = (v0' << 24) + (v1' << 16) + (v2' << 8) + v3'
        v0' = toInteger v0
        v1' = toInteger v1
        v2' = toInteger v2
        v3' = toInteger v3

get1 (d0:ds) = (build1 d0, ds)
get2 (d0:d1:ds) = (build2 d0 d1, ds)
get3 (d0:d1:d2:ds) = (build3 d0 d1 d2, ds)
get4 (d0:d1:d2:d3:ds) = (build4 d0 d1 d2 d3, ds)
getk = splitAt
\end{code}

\begin{code}
decode :: [DVIByte] -> [String]
decode [] = []
decode (d:ds)
    | d < 128 = (("char " ++ (show d) ++ "(" ++ [chr $ fromInteger $ toInteger d] ++ ")"):decode ds)


decode (138:ds) = ("nop":decode ds)
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

decode (d:ds)
    | d >= 171 && d < (171 + 64) = (("fnt_num" ++ (build1 (d-171))):decode ds)
    -- 171 + 64 = 235

decode (d:r0)
    | d == 243 = fnt_decode get1
    | d == 244 = fnt_decode get2
    | d == 245 = fnt_decode get3
    | d == 246 = fnt_decode get4
    where
        fnt_decode get = (("fnt_def" ++ k ++ c ++ s ++ d ++ a ++ " " ++ (word8sToChars path)):decode rest) where
            (k,r1) = get r0
            (c,r2) = get4 r1
            (s,r3) = get4 r2
            (d,r4) = get4 r3
            (a,r5) = get1 r4
            (l,r6) = get1 r5
            (path,rest) = getk ((read $ tail a)+(read $ tail l)) r6

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

\begin{code}
decode (d:ds) = (("unknown" ++ (build1 d)):decode ds)
\end{code}<++>

\begin{code}
main = do
    args <- getArgs
    input <- B.readFile (args !! 0)
    putStr $ unlines $ decode $ map (fromInteger . toInteger) $ B.unpack input
\end{code}
