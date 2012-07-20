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
getRegister :: String -> Integer -> HexEnvironment -> E.HexType
getRegister class_ rid = E.lookupWithDefault errormsg (class_++"-register:"++show rid)
    where errormsg = error $ concat ["hex.getRegister: lookup of ",class_, " ", show rid," failed."]

setRegister class_ wrapper isglobal rid val =
        ins isglobal (class_++"-register:"++show rid) (wrapper val)
    where
        ins True = E.globalinsert
        ins False = E.insert

getCount :: Integer -> HexEnvironment -> Integer
getCount cid = (\(E.HexInteger i) -> i) . getRegister "count" cid
setCount = setRegister "count" E.HexInteger

getDimen did = (\(E.HexDimen d) -> d) . getRegister "dimen" did
setDimen = setRegister "dimen" E.HexDimen

getSkip sid = (\(E.HexGlue g) -> g) . getRegister "skip" sid
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
processinputs ((SetIParameterCommand _cid _val):r) e = processinputs r e
processinputs ((SetDParameterCommand _cid _val):r) e = processinputs r e
processinputs ((SetCountCommand cid val):r) e = processinputs r (setCount False cid val e)
processinputs ((SetDimenCommand cid val):r) e = processinputs r (setDimen False cid val e)
processinputs ((SetSkipCommand cid val):r) e = processinputs r (setSkip False cid val e)
processinputs ((AdvanceCountCommand isg cid val):r) e = processinputs r (setCount isg cid (v + val') e)
    where
        v = getCount cid e
        val' = either id (`getCount` e) val
processinputs ((AdvanceDimenCommand isg did val):r) e = processinputs r (setDimen isg did (v `dplus` val') e)
    where
        v = getDimen did e
        val' = either id (`getDimen` e) val
processinputs ((AdvanceSkipCommand isg did val):r) e = processinputs r (setSkip isg did (v `dplus` val') e)
    where
        v = getSkip did e
        val' = either id (`getSkip` e) val
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
    r <- processinputs cs e
    return ((SelectfontCommand fontindex fontinfo):r)
\end{code}

\begin{code}
processinputs ((InternalCommand _ _ (SetMathFontHCommand fname fam fs)):cs) e = do
    let E.HexInteger fontindex = fromJust $ E.lookup ("font-index:"++fname) e
    let E.HexFontInfo fontinfo = fromJust $ E.lookup ("font:"++fname) e
    r <- processinputs cs e
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
