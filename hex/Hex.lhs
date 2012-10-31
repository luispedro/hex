\section{Hex Driver}

This is the main driver of the programme.

\begin{code}
module Hex
    ( processinputs
    , readFont
    ) where
import System.IO.Error hiding (catch)
import Prelude hiding (catch)
import System.IO.Unsafe
import System.FilePath.Posix
import Control.Monad
import Control.Exception
import System.Process (readProcess)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Maybe

import Macros
import ParseTFM
import Fonts
import DVI
import Measures
import Tokens (updateCharStream, TokenStream)
import CharStream (prequeue)
import qualified Environment as E
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
type HexEnvironment = E.Environment String E.HexType
\end{code}

Processing \tex{\\input} is achieved by prequeueing the characters in the input
file.

\begin{code}
prequeueChars :: (String,LT.Text) -> TokenStream -> TokenStream
prequeueChars q st = updateCharStream st (prequeue q)
\end{code}

Often we will have to search for a file in several locations and read the first
one we find. The function \haskell{readOneOf} achieves this:

\begin{code}
readOneOf [] = error "hex.hex.readOneOf: empty set"
readOneOf [n] = LT.readFile n
readOneOf (n:ns) = (LT.readFile n) `catch` (\e -> if isDoesNotExistError e then readOneOf ns else ioError e)
\end{code}

To read and write fonts:

\begin{code}
fontpath :: String -> IO String
fontpath fname = liftM init $ readProcess "kpsewhich" [fname ++ ".tfm"] []

readFont :: String -> IO (FontDef,FontInfo)
readFont fname = do
    absname <- fontpath fname
    fontstr <- B.readFile absname
    return $ parseTFM fname fontstr
\end{code}

\begin{code}
_fontId :: IORef Integer
_fontId = unsafePerformIO $ newIORef 0

nextFontId :: IO Integer
nextFontId = atomicModifyIORef _fontId (\n -> (n+1,n))
\end{code}

\code{processinputs} processes the special commands in the \code{Command}
stream.


The implementation of \tex{\\count} is hidden behind a few helper functions:
\begin{code}
getRegister :: (Show a) => String -> Quantity a -> HexEnvironment -> E.HexType
getRegister class_ rid = E.lookupWithDefault errormsg (class_++ename rid)
    where
        errormsg = error $ concat ["hex.getRegister: lookup of ",class_, " ", show rid," failed."]

setRegister class_ wrapper isglobal rid val =
        ins isglobal (class_++ename rid) (wrapper val)
    where
        ins True = E.globalinsert
        ins False = E.insert
ename (QConstant _) = error "ename of QConstant"
ename (QRegister r) = "register:"++show r
ename (QInternal i) = "internal:"++i
ename (QScaled _ q) = "scaled:"++ename q

getCount :: Quantity Integer -> HexEnvironment -> Integer
getCount (QConstant v) = const v
getCount cid = (\(E.HexInteger i) -> i) . getRegister "count" cid
setCount = setRegister "count" E.HexInteger
\end{code}

To handle \code{Dimen}s, we need to handle the case where the dimension depends
on the font (if the unit is em, ex, or mu):
\begin{code}
getDimen (QConstant v) e = dimenFromFont v (currentFont e)
getDimen did e = (\(E.HexDimen d) -> d) . getRegister "dimen" did $ e

currentFont :: HexEnvironment -> FontInfo
currentFont = snd . (\(E.HexFontInfo f) -> f) . fromJust . E.lookup "current-font"

dimenFromFont :: UDimen -> FontInfo -> Dimen
dimenFromFont (UDimen v UnitEm) f = v `dscalef` (dimenFromFloatingPoints . quad $ f)
dimenFromFont (UDimen v UnitMu) f = (v / 8) `dscalef` (dimenFromFloatingPoints . quad $ f)
dimenFromFont ud _f = asDimen ud

setDimen = setRegister "dimen" E.HexDimen
\end{code}

Glues are just builtup of multiple dimen lookups:
\begin{code}
getSkip :: Quantity UGlue -> HexEnvironment -> Glue
getSkip (QConstant v) e =
            Glue
                (getDimen (QConstant . usize $ v) e)
                (getDimen (QConstant . ushrinkage $ v) e)
                (getDimen (QConstant . uexpandable $ v) e)
                (uinfLevel v)
getSkip sid e = (\(E.HexGlue g) -> g) . getRegister "skip" sid $ e
setSkip = setRegister "skip" E.HexGlue
\end{code}
In particular, it process \tex{\\bye}, \tex{\\input}, and
\tex{\\message}.

We start with the basics:

\begin{code}
processinputs :: [Command] -> HexEnvironment -> IO [Command]
processinputs [] _ = return []
\end{code}

\code{setcount} and \code{setdimen} are both very simple:
\begin{code}
processinputs ((SetCountCommand isg cid val):r) e = processinputs r (setCount isg cid val e)
processinputs ((SetDimenCommand isg cid val):r) e = processinputs r (setDimen isg cid (asDimen val) e)
processinputs ((SetSkipCommand isg cid val):r) e = processinputs r (setSkip isg cid val e)
processinputs ((AdvanceCountCommand isg cid val):r) e = processinputs r (setCount isg cid (v + val') e)
    where
        v = getCount cid e
        val' = getCount val e
processinputs ((AdvanceDimenCommand isg did val):r) e = processinputs r (setDimen isg did (v `dplus` val') e)
    where
        v :: Dimen
        v = getDimen did e
        val' = getDimen val e
processinputs ((AdvanceSkipCommand isg did val):r) e = processinputs r (setSkip isg did (v `gplus` val') e)
    where
        gplus (Glue b0 st0 sh0 i0) (Glue b1 st1 sh1 i1) = Glue (b0 `dplus` b1) (st0`dplus` st1) (sh0 `dplus` sh1) (i0 + i1)
        v = getSkip did e
        val' = getSkip val e
\end{code}

The simplest command is the \tex{\\bye} command. Just stop everything, we are
done.

\begin{code}
processinputs ((InternalCommand _ _ ByeCommand):_) _ = return []
\end{code}

Another simple commmand is the \code{MessageCommand}, which outputs its message.
\begin{code}
processinputs ((InternalCommand _ _ (MessageCommand msg)):cs) e = (putStrLn msg) >> (processinputs cs e)
\end{code}

At the moment, deprecated commands are handled like messages:
\begin{code}
processinputs ((InternalCommand _ _ (DeprecatedCommand msg)):cs) e = (putStrLn msg) >> (processinputs cs e)
\end{code}

\code{ErrorCommand} is similar, except we stop after errors:
\begin{code}
processinputs ((InternalCommand _ _ (ErrorCommand msg)):_) _e = (putStrLn msg) >> return []
\end{code}

Loading a font involves replacing the string name in the command stream by the
loaded font object

\begin{code}
processinputs ((InternalCommand _ _ (LoadfontHCommand fname)):cs) e = do
    font <- readFont fname
    next <- nextFontId
    let e' = E.globalinsert ("font-index:" ++ fname)  (E.HexInteger next) e
    let e'' = E.globalinsert ("font:" ++ fname)  (E.HexFontInfo font) e'
    r <- processinputs cs e''
    return ((OutputfontCommand font):r)
\end{code}

Selecting a font is a couple of environment lookups:

\begin{code}
processinputs ((InternalCommand _ _ (SelectfontHCommand fname)):cs) e = do
    let E.HexInteger fontindex = fromJust $ E.lookup ("font-index:"++fname) e
    let E.HexFontInfo fontinfo = fromJust $ E.lookup ("font:"++fname) e
    let e' = E.globalinsert "current-font" (E.HexFontInfo fontinfo) e
    r <- processinputs cs e'
    return ((SelectfontCommand fontindex fontinfo):r)
\end{code}

\begin{code}
processinputs ((InternalCommand _ _ (SetMathFontHCommand fname fam fs)):cs) e = do
    let E.HexInteger fontindex = fromJust $ E.lookup ("font-index:"++fname) e
        E.HexFontInfo fontinfo = fromJust $ E.lookup ("font:"++fname) e
        e' = E.globalinsert "current-mfont" (E.HexFontInfo fontinfo) e
    r <- processinputs cs e'
    return ((SetMathFontCommand fontindex fontinfo fam fs):r)
\end{code}

Looking up a value in a register just involves switching the command stream:

\begin{code}
processinputs ((InternalCommand _ _ (LookupCountHCommand cid (Lookup f))):_) e = processinputs (f $ getCount cid e) e
processinputs ((InternalCommand _ _ (LookupDimenHCommand did (Lookup f))):_) e = processinputs (f $ getDimen did e) e
\end{code}

Finally, the \code{InputCommand} finds the input file and queues it in

\begin{code}
processinputs ((InternalCommand env rest (InputCommand nfname)):_) e = do {
            nextfile <- readOneOf possiblefiles;
            processinputs (expandE env $ prequeueChars (nfname,nextfile) rest) (addfileenv nfname e);
        } `catch` printerror
    where
        printerror err =
            if isDoesNotExistError err then do
                putStrLn ("Could not open file: `" ++ nfname ++ "`")
                putStrLn ("\tAttempted to open one of: " ++ (concatMap (++" ") possiblefiles))
                processinputs (expandE env rest) e
            else
                ioError err
        possiblefiles
            | isAbsolute nfname = [nfname]
            | hasExtension nfname = do
                dir <- searchpath
                return (dir </> nfname)
            | otherwise = do
                dir <- searchpath
                ext <- ["hex", "tex"]
                return (dir </> (nfname <.> ext))
        searchpath = [currentdir, "."]
        currentdir = case E.lookup "currentfile" e of
            Just (E.HexString f) -> fst $ splitFileName f
            Nothing -> ""
            _ -> error "hex.hex.currentdir: environment is messed up"
        addfileenv = (E.globalinsert "currentfile") . E.HexString
\end{code}

Finally, the default case is to just pass it on.
\begin{code}
processinputs (c:cs) e = do
    cs' <- (processinputs cs e)
    return $ (c:cs')
\end{code}
