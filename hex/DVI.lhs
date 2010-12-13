\section{DVI: Device Independent Files}
\begin{code}
module DVI where
\end{code}

Device independent files are by now a legacy technology, but they are very
simple to implement and are supported by \TeX. Therefore, in order to be full
\TeX{} compliant, we would need to implement them.

\begin{code}
import qualified Data.ByteString.Lazy as B
import Control.Monad.State (State, modify, get, put)
import Data.Word
\end{code}

A DVI file is a sequence of 8-bit bytes.

\begin{code}
type DVIByte = Word8
\end{code}

In order to implement the backlinking, we need to keep track of the current
position in the stream and, most importantly, the previous \emph{bop} position.

\begin{code}
data DVIStream = DVIStream { stream :: B.ByteString
                           , pos :: Integer
                           , lastBop :: Integer
                           } deriving (Eq)
\end{code}

The main function puts a single byte into the stream:

\begin{code}
putByte :: DVIByte -> State DVIStream ()
putByte b = modify putByteInto
    where
        putByteInto st@(DVIStream {}) = st { stream=(B.cons b $ stream st), pos=((pos st)+1) }
\end{code}

Now, a few functions to retrieve and manipulate the state:

\begin{code}
getCurPos :: State DVIStream Integer
getCurPos = get >>= (return . pos)
getLastBop = get >>= (return . lastBop)
putLastBop b = modify (\st -> st { lastBop=b })

putn :: Integer -> Integer -> State DVIStream ()
putn 0 b = return ()
putn n b = do
    putByte $ fromInteger $ b `mod` 8
    putn (n-1) (b `div` 8)
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
push = put1 141
pop = put1 142
right1 w = (put1 143) >> (put1 w)
right2 w = (put1 144) >> (put2 w)
right3 w = (put1 145) >> (put3 w)
right4 w = (put1 146) >> (put4 w)

down1 a = (put1 157) >> (put1 a)
down2 a = (put1 158) >> (put2 a)
down3 a = (put1 159) >> (put3 a)
down4 a = (put1 160) >> (put4 a)

pre i num den mag k x = do
    put1 247
    put1 i
    put4 num
    put4 den
    put4 mag
    put1 mag
    putk k x

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
    putk n223 $ map fromInteger [223,223,223,223]

\end{code}
