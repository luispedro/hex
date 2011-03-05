\section{DVI: Device Independent Files}
\begin{code}
module DVI where
\end{code}

Device independent files are by now a legacy technology, but they are very
simple to implement and are supported by \TeX. Therefore, in order to be full
\TeX{} compliant, we would need to implement them.

\begin{code}
import Measures

import qualified Data.ByteString.Lazy as B
import Control.Monad.State (State, modify, get, put)
import Data.Word
import Data.Char
import Data.Ratio
\end{code}

A DVI file is a sequence of 8-bit bytes.

\begin{code}
type DVIByte = Word8
\end{code}

In order to implement the backlinking, we need to keep track of the current
position in the stream and, most importantly, the previous \emph{bop} position.

\begin{code}
data FontDef = FontDef  { checkSum :: Integer
                        , scale :: Integer
                        , designSize :: Integer
                        , areaLength :: Integer
                        , localPath :: Integer
                        , path :: [DVIByte]
                        } deriving (Eq, Show)

data DVIStream = DVIStream { rstream :: B.ByteString
                           , pos :: Integer
                           , lastBop :: Integer
                           , totalPages :: Integer
                           , maxV :: Integer
                           , maxH :: Integer
                           , curPush :: Integer
                           , maxPush :: Integer
                           , fontDefs :: [FontDef]
                           } deriving (Eq)
\end{code}

We store the stream in reverse form for performance. This way, we are always
appending to the front (which is fast).

\begin{code}
stream = B.reverse. rstream
\end{code}

Initially the stream starts empty:

\begin{code}
emptyStream = DVIStream
            { rstream=B.empty
            , pos=0
            , lastBop=(-1)
            , totalPages=0
            , maxV=0
            , maxH=0
            , curPush=0
            , maxPush=0
            , fontDefs=[]
            }
\end{code}

The main function puts a single byte into the stream:

\begin{code}
putByte :: DVIByte -> State DVIStream ()
putByte b = modify putByteInto
    where
        putByteInto st@(DVIStream {}) = st { rstream=(B.cons b $ rstream st), pos=((pos st)+1) }
\end{code}

Now, a few functions to retrieve and manipulate the state:

\begin{code}
getCurPos :: State DVIStream Integer
getCurPos = get >>= (return . pos)
getLastBop = get >>= (return . lastBop)
putLastBop b = modify (\st -> st { lastBop=b })

getMaxPush = get >>= (return . maxPush)
pushOne = modify (\st -> let nd = (curPush st) + 1 in  st { curPush=nd, maxPush=(max (maxPush st) nd) })
popOne = modify (\st -> st { curPush=((curPush st) - 1) })

getTotalPages = get >>= (return . totalPages)
putTotalPages p = modify (\st -> st { totalPages=p })
getFonts = get >>= (return . fontDefs)
getNFonts :: State DVIStream Integer
getNFonts = get >>= (return . toInteger . length . fontDefs)
appendFont fnt = modify appendFont
    where
        appendFont st@(DVIStream {fontDefs=fs}) = st { fontDefs=(fs ++ [fnt]) }

putn :: Integer -> Integer -> State DVIStream ()
putn 0 b = return ()
putn n b = do
    putn (n-1) (b `div` 256)
    putByte $ fromInteger $ b `mod` 256
put1 = putn 1
put2 = putn 2
put3 = putn 3
put4 = putn 4
putk 0 _ = return ()
putk n (x:xs) = (put1 x) >> (putk (n-1) xs)
\end{code}

Now, we set down the DVI format (from the TeX Program source code). Our
function names correspond to DVI commands, with the exception of the
\code{setchar\emph{n}} family which is mapped to a single function
\code{setchar} which takes an argument.

\begin{code}
setchar c | c < 128 = put1 c
nop = 138
bop c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 p = do
        thisBop <- getCurPos
        put1 139
        put4 c0
        put4 c1
        put4 c2
        put4 c3
        put4 c4
        put4 c5
        put4 c6
        put4 c7
        put4 c8
        put4 c9
        lastBop <- getLastBop
        put4 lastBop
        putLastBop thisBop

eop = put1 140
push = (put1 141) >> pushOne
pop = (put1 142) >> popOne
right1 w = (put1 143) >> (put1 w)
right2 w = (put1 144) >> (put2 w)
right3 w = (put1 145) >> (put3 w)
right4 w = (put1 146) >> (put4 w)

down1 a = (put1 157) >> (put1 a)
down2 a = (put1 158) >> (put2 a)
down3 a = (put1 159) >> (put3 a)
down4 a = (put1 160) >> (put4 a)

w0 = put1 147
w1 b = (put1 148) >> (put1 b)
w2 b = (put1 149) >> (put2 b)
w3 b = (put1 150) >> (put3 b)
w4 b = (put3 151) >> (put4 b)

x0 = put1 152
x1 b = (put1 153) >> (put1 b)
x2 b = (put1 154) >> (put2 b)
x3 b = (put1 155) >> (put3 b)
x4 b = (put3 156) >> (put4 b)

pre i num den mag k x = do
    put1 247
    put1 i
    put4 num
    put4 den
    put4 mag
    put1 k
    putk k x

fnt_def code ksize k c s d a l n = do
    put1 code
    ksize k
    put4 c
    put4 s
    put4 d
    put1 a
    put1 l
    putk (a+l) n

fnt_def1 = fnt_def 243 put1
fnt_def2 = fnt_def 244 put2
fnt_def3 = fnt_def 245 put3
fnt_def4 = fnt_def 246 put4

fnt_num n = put1 $ 171 + n


post p num den mag l u s t = do
    put1 248
    put4 p
    put4 num
    put4 den
    put4 mag
    put4 l
    put4 u
    put2 s
    put2 t
post_post q i n223 = do
    put1 249
    put4 q
    put1 i
    putk n223 $ map fromInteger [223,223,223,223,223,223,223]

\end{code}

Some constants to match \TeX.
\begin{code}
tex_num = 25400000 :: Integer
tex_den = 473628672 :: Integer
tex_mag = 1000 :: Integer

pointsTointernal :: Integer -> Integer
pointsTointernal = id
\end{code}

There are two parameters that I'm still not sure how to compute, so I'll just
set them as I saw in a file generated by \TeX:

\begin{code}
magic_s = 0x29b33da
magic_u = 0x1d5c147
\end{code}

Based on these low-level functions, we define a higher level interface.

\begin{code}
startwcomment comment = pre 2 tex_num tex_den tex_mag k x
    where
        k = toInteger $ length comment
        x = map (toInteger . ord) comment
startfile = startwcomment "Written by hex"

newpage = do
    page <- getTotalPages
    bop (page + 1) 0 0 0 0 0 0 0 0 0 (page + 1)
    putTotalPages (page + 1)
    return (page + 1)

putstr [] = return ()
putstr (c:cs) = (put1 $ toInteger $ ord c) >> (putstr cs)
move_down = down4 . pointsTointernal . nrScaledPoints
move_right = right4 . pointsTointernal . nrScaledPoints

defineFont fnt@(FontDef c s d a l t) = do
    nfonts <- getNFonts
    fnt_def1 nfonts c s d a l (map toInteger t)
    appendFont fnt
    return nfonts

selectFont = fnt_num

endfile = do
    lastBop <- getLastBop
    fonts <- getFonts
    npages <- getTotalPages
    q <- getCurPos
    mpd <- getMaxPush
    post lastBop tex_num tex_den tex_mag magic_s magic_u mpd npages
    putfonts 0 fonts
    pos <- getCurPos
    post_post q 2 (if (pos `mod` 4) == 0 then 4 else 8 - (pos `mod` 4))
    where
        putfonts _ [] = return ()
        putfonts n (f:fs) = putfont n f >> (putfonts (n+1) fs)
        putfont n (FontDef c s d a l path) = do
            fnt_def1 n c s d a l (map toInteger path)
\end{code}
