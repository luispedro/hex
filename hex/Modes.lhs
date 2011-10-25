\section{Modes}
\begin{code}
module Modes
    ( vMode
    , _paragraph
    , ModeState(..)
    ) where

import qualified Environment as E
import qualified Fonts as F
import Chars
import Macros
import Measures
import Boxes
import Linebreak
import FixWords

import Control.Monad.State
\end{code}

Some commands are v-mode commands, other h-mode commands. We begin with a
series of definitions to distinguish the two.

\begin{code}
isVCommand :: String -> Bool
isVCommand "\\vspace" = True
isVCommand _ = False
\end{code}

The implementation of the modes is in the state monad. It includes both the
environment and the input stream.

\begin{code}
data ModeState = ModeState
            { environment :: E.Environment String E.HexType
            , commands :: [Command]
            }
type Modes a = State ModeState a
\end{code}

Now, we define several helpler functions to manipulate the stream.
\code{getCommand} unconditionally gets the next command, whilst \code{tryPeek}
just returns a \code{Maybe Command} and does not touch the stream.

\begin{code}
getCommand :: Modes Command
getCommand = do
        st <- get
        modify dropC
        return $ head $ commands st
    where
        dropC (ModeState e (_:cs)) = ModeState e cs
        dropC (ModeState _ []) = error "hex.Modes.dropC: empty stream"
tryPeek = do
    st <- get
    case commands st of
        [] -> return Nothing
        (c:_) -> return (Just c)
\end{code}

Now a few functions to retrieve and manipulate the environment:

\begin{code}
getEnvironment :: Modes (E.Environment String E.HexType)
getEnvironment = gets environment

pushE :: Modes ()
pushE = modify (\st@ModeState{ environment=e } -> st { environment=(E.push e) })
popE :: Modes ()
popE = modify (\st@ModeState{ environment=e } -> st { environment=(E.pop e) })
\end{code}

These are necessary to read numbers and units from the stream. Error handling
is poor.

\begin{code}
readNumber :: Modes Integer
readNumber = do
        cs <- readWhile isDigit
        return $ toNumber cs
    where
        isDigit (CharCommand (TypedChar c Other)) = (c `elem` "0123456789")
        isDigit _ = False
        toNumber :: [Command] -> Integer
        toNumber digits = val
            where
                val = read $ map cvalue digits
                cvalue (CharCommand (TypedChar c Other)) = c
                cvalue _ = error "hex.Modes.readNumber.cvalue: wrong type"

readWhile cond = do
    mc <- tryPeek
    case mc of
        Nothing -> return []
        Just c ->
            if cond c
                then do
                    void getCommand
                    r <- readWhile cond
                    return (c:r)
                else
                    return []

readUnits :: Modes Unit
readUnits = do
    c0 <- getCommand
    c1 <- getCommand
    e <- getEnvironment
    return $ unit e c0 c1

unit _env (CharCommand (TypedChar c0 Letter)) (CharCommand (TypedChar c1 Letter)) = unit' [c0,c1]
    where
        unit' "pt" = UnitPt
        unit' "px" = UnitPx
        unit' "en" = error "hex.unit' en not implemented"
        unit' "em" = error "hex.unit' em not implemented"
        unit' _ = error "hex.unit' could not match"
unit _e _c0 _c1 = error "hex.readUnits: could not read unit"

readDimen = do
    n <- readNumber
    u <- readUnits
    return $ dimenFromUnit (fromInteger n) u
\end{code}

Now, we come to the actual code. \code{vModeM} implements v-mode (in the
monad). It just checks whether the next command is a v-command. If so, it calls
\code{vMode1}, otherwise it call \code{hMode}:

\begin{code}
vModeM :: Modes [VBox]
vModeM = do
    c <- tryPeek
    case c of
        Nothing -> return []
        Just (PrimitiveCommand csname) ->
            if isVCommand csname
                then vMode1
                else hMode
        _ -> hMode
\end{code}

\code{vMode1} handles one vertical mode command. Because of the way it is
called, we know that it must be a v-mode command:

\begin{code}
vMode1 :: Modes [VBox]
vMode1 = do
    c <- getCommand
    case c of
        PrimitiveCommand "\\vspace" -> do
            d <- readDimen
            r <- vModeM
            return ((Box V d zeroDimen zeroDimen (Kern d)):r)
        _ -> fail (concat ["hex.Modes.vMode1: Can only handle PrimitiveCommands that are v-mode commands. Got ", show c, "."])
\end{code}

Now that we have dealt with vertical mode, we must deal with the horizontal.
The first function puts down a single character to form an \code{HElement}:

\begin{code}
setCharacter :: TypedChar -> Modes HElement
setCharacter tc = (getEnvironment >>= (return . setCharacter'))
    where
        setCharacter' e = toHElement tc
            where
                Just (E.HexFontInfo (_,fnt)) = E.currentfont e
                (F.SpaceInfo spS spSt spShr) = F.spaceInfo fnt
                f2d = dimenFromFloatingPoints . fixToFloat
                toHElement (TypedChar _ Space) = EGlue $ Glue H (f2d spS) (f2d spSt) (f2d spShr) 0
                toHElement (TypedChar c _) = EBox $ Box
                                        { boxType=H
                                        , width=(f2d w)
                                        , height=(f2d h)
                                        , depth=(f2d d)
                                        , boxContents=typesetChar c
                                        } where (w,h,d) = F.widthHeightDepth fnt c
                toHElement c = error (concat ["hex.Modes.setCharacter': Can only handle CharCommand (got ", show c, ")"])
\end{code}

Building up, \code{_paragraph} gets a single paragraph as a list of
\code{HElement}s (but they are still just a list).

\begin{code}
_paragraph :: Modes [HElement]
_paragraph = do
    mc <- tryPeek
    case mc of
        Nothing -> return []
        Just c -> do
            void getCommand
            if isParagraphBreak c
                then return []
                else case c of
                    PushCommand -> (pushE >> _paragraph)
                    PopCommand -> (popE >> _paragraph)
                    CharCommand tc -> do
                        h <- setCharacter tc
                        r <- _paragraph
                        return (h:r)
                    _ -> fail "hex.Modes._paragraph: Unhandled case"
\end{code}

Paragraphs can be terminated in three ways:
\begin{enumerate}
\item end of file,
\item \tex{\\par},
\item any vertical mode command.
\end{enumerate}

The \code{isParagraphBreak} function implements this logic:

\begin{code}
isParagraphBreak (PrimitiveCommand "\\par") = True
isParagraphBreak (PrimitiveCommand csname) = isVCommand csname
isParagraphBreak _ = False
\end{code}

Given the list of \code{HElement}s, we need to set them in lines, represented
by \code{VBox}es. \code{typesetParagraph} performs this function:

\begin{code}
typesetParagraph :: [HElement] -> Modes [VBox]
typesetParagraph p = (getEnvironment >>= return . typesetParagraph')
    where
        typesetParagraph' env = (spreadlines baselineskip $ breakintolines linewidth p)
            where
                Just (E.HexDimen linewidth) = E.lookup "textwidth" env
                Just (E.HexScaledNumber baselineskip) = E.lookup "baselineskip" env

\end{code}
\begin{code}
spreadlines :: Scaled -> [VBox] -> [VBox]
spreadlines _ [] = []
spreadlines baselineskip (v:vs) = (v:k:spreadlines baselineskip vs)
    where
        k = Box { boxType=V, height=ht, depth=zeroDimen, width=zeroDimen, boxContents=(Kern ht) }
        ht = (height v) `dmul` (scaledToRational baselineskip)

\end{code}

\code{hMode} implements horizontal mode, whose output is a series of vertical
boxes. The implementation is based on what we have above and is pretty trivial.

\begin{code}
hMode :: Modes [VBox]
hMode = do
    p <- _paragraph
    tp <- typesetParagraph p
    r <- vModeM
    return (tp ++ r)
\end{code}

Finally, we hide it all behind a pure interface:

\begin{code}
vMode :: E.Environment String E.HexType -> [Command] -> [VBox]
vMode e cs = evalState vModeM (ModeState e cs)
\end{code}
