\section{Modes}
\begin{code}
module Modes
    ( vMode
    , ModeState(..)
    , _vModeM
    , _paragraph
    ) where

import qualified Environment as E
import qualified Fonts as F
import Chars
import Macros
import Measures
import Boxes
import Linebreak
import FixWords

import Text.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Prim as Prim
import Control.Monad
import Control.Applicative
\end{code}

The implementation of the modes is as a Parsec based on \code{[Command]}. The
state is the environment:

\begin{code}
data ModeState = ModeState { environment :: E.Environment String E.HexType }
type Modes a = Parsec [Command] ModeState a
\end{code}

Now a few functions to retrieve and manipulate the environment:
\begin{code}
environmentM :: Modes (E.Environment String E.HexType)
environmentM = do
    s <- getState
    return $ environment s

pushE :: Modes ()
pushE = modifyState (\st@ModeState{ environment=e } -> st { environment=(E.push e) })
popE :: Modes ()
popE = modifyState (\st@ModeState{ environment=e } -> st { environment=(E.pop e) })
\end{code}

Small helpers, to match commands that fulfil a certain condition (like
equality) and to match character categories:
\begin{code}
incCol pos _ _  = incSourceColumn pos $ sourceColumn pos
matchf f = Prim.tokenPrim show incCol testChar
    where testChar t = if f t then Just t else Nothing

match c = matchf (==c)

matchcat :: CharCategory -> Modes Command
matchcat cat = matchf testCat
    where
      testCat (CharCommand (TypedChar _ cat')) = cat == cat'
      testCat _ = False
\end{code}

A simple case is \code{charcommand}:
\begin{code}
charcommand = matchf (\c -> case c of { CharCommand _ -> True; _ -> False })
\end{code}

These are necessary to read numbers and units from the stream. Error handling
is poor.

\begin{code}
readNumber :: Modes Integer
readNumber = do
        cs <- many (matchf isDigit)
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
readUnits :: Modes Unit
readUnits = do
    c0 <- matchf (const True)
    c1 <- matchf (const True)
    e <- environmentM
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

Now, we come to the actual code. \code{_vModeM} implements v-mode (in the
monad).

\begin{code}
_vModeM :: Modes [VBox]
_vModeM = (eof >> return []) <|> do
    v <- vMode1
    r <- _vModeM
    return (v++r)
\end{code}

\code{vMode1} handles one vertical mode command.

\begin{code}
vMode1 :: Modes [VBox]
vMode1 = vspace <|> outputfont <|> hMode

outputfont = do
    OutputfontCommand fontinfo <- matchf (\c -> case c of { OutputfontCommand _ -> True; _ -> False })
    let onode = Box V zeroDimen zeroDimen zeroDimen (DefineFontContent fontinfo)
    return [onode]

vspace = do
    void $ match (PrimitiveCommand "\\vspace")
    d <- readDimen
    return [Box V d zeroDimen zeroDimen (Kern d)]
\end{code}

Now that we have dealt with vertical mode, we must deal with the horizontal.
The first function puts down a single character to form an \code{HElement}:

\begin{code}
setCharacter :: Modes HElement
setCharacter = do
    CharCommand tc <- charcommand
    e <- environmentM
    return $ toHElement e tc

toHElement e = toHElement'
    where
        (fidx,(_,fnt)) = E.currentfont e
        (F.SpaceInfo spS spSt spShr) = F.spaceInfo fnt
        f2d = dimenFromFloatingPoints . fixToFloat
        toHElement' (TypedChar c cat)
            | cat == Space = EGlue $ Glue H (f2d spS) (f2d spSt) (f2d spShr) 0
            | otherwise = EBox $ Box
                            { boxType=H
                            , width=(f2d w)
                            , height=(f2d h)
                            , depth=(f2d d)
                            , boxContents=(CharContent c fidx)
                            } where (w,h,d) = F.widthHeightDepth fnt c
        toHElement' c = error (concat ["hex.Modes.setCharacter': Can only handle CharCommand (got ", show c, ")"])
\end{code}

Selecting a font is easy, just set the font in the environment:
\begin{code}
selectfont = do
    SelectfontCommand i fontinfo <- matchf (\t -> case t of { SelectfontCommand _ _ -> True ; _ -> False })
    modifyState (\st@ModeState{ environment=e } -> st { environment=(E.setfont i fontinfo e) })
    return ()
\end{code}


Building up, \code{_paragraph} gets a single paragraph as a list of
\code{HElement}s (but they are still just a list).

\begin{code}
_paragraph :: Modes [HElement]
_paragraph =
    (eof >> return []) <|>
    (match (PrimitiveCommand "\\par") >> return []) <|>
    (match  PushCommand >> pushE >> _paragraph) <|>
    (match  PopCommand >> popE >> _paragraph) <|>
    (match  MathShiftCommand >> mMode >>= typesetMListM) <|>
    (setCharacter >>= (\h -> _paragraph >>= return . (h:))) <|>
    (selectfont >> _paragraph) <|>
    return []
\end{code}

Given the list of \code{HElement}s, we need to set them in lines, represented
by \code{VBox}es. \code{typesetParagraph} performs this function:

\begin{code}
typesetParagraph env p = (spreadlines baselineskip $ breakintolines linewidth p)
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
    e <- environmentM
    return $ typesetParagraph e p
\end{code}

Now comes math mode:

\begin{code}
data MList = MAtom { center :: MList, sup :: Maybe MList, sub :: Maybe MList }
        | MChar Char
        | MRel MList
        | MListList [MList]
        deriving (Eq, Show)

\end{code}

Now get match a single character as a \code{MChar}:
\begin{code}
singlechar :: Modes MList
singlechar = do
    CharCommand (TypedChar c _) <- charcommand
    return $ MChar c
\end{code}

Now we build up on this:
\begin{code}
inbraces = (match PushCommand *> mlist <* match PopCommand)
node :: Modes MList
node = singlechar <|> inbraces

matom :: Modes MList
matom = do
    c <- node
    down <- optionMaybe (matchcat SubScript >> node)
    up <- optionMaybe (matchcat Superscript >> node)
    return $ MAtom c down up

mlist = (many matom >>= return . MListList)
\end{code}

End of math mode:

\begin{code}
eomath :: Modes ()
eomath = do
    void $ match MathShiftCommand

mMode :: Modes MList
mMode = mlist <* eomath
\end{code}

Typesetting math:
\begin{code}
typesetMListM :: MList -> Modes [HElement]
typesetMListM ml = do
    e <- environmentM
    return $ typesetMList e ml

typesetMList e = set
    where
        set :: MList -> [HElement]
        set (MChar c) = [toHElement e (TypedChar c Letter)]
        set (MListList ml)= concat $ set `map` ml
        set (MRel ml)= set ml
        set MAtom { center=c, sup=up, sub=down} = set c ++ setmaybe up ++ setmaybe down
        setmaybe Nothing = []
        setmaybe (Just ml) = set ml
\end{code}

Finally, we hide it all behind a pure interface:

\begin{code}
vMode :: E.Environment String E.HexType -> [Command] -> [VBox]
vMode e cs = case runP _vModeM (ModeState e) "input" cs of
    Right res -> res
    Left err -> error $ show err
\end{code}
