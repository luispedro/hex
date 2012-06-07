\section{Modes}
\begin{code}
module Modes
    ( vMode
    , _vModeM
    , _paragraph
    ) where

import qualified Environment as E
import Chars
import Macros
import Measures
import Boxes
import Linebreak
import Maths

import Text.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Prim as Prim
import Control.Monad
import Control.Applicative
\end{code}

The implementation of the modes is as a Parsec based on \code{[Command]}. The
state is the environment:

\begin{code}
type ModeState = E.Environment String E.HexType
type Modes a = Parsec [Command] ModeState a
\end{code}

Now a few functions to retrieve and manipulate the environment:
\begin{code}
environmentM :: Modes (E.Environment String E.HexType)
environmentM = getState

pushE :: Modes ()
pushE = modifyState E.push
popE :: Modes ()
popE = modifyState E.pop
\end{code}

Small helpers, to match commands that fulfil a certain condition (like
equality) and to match character categories:
\begin{code}
incCol pos _ _  = incSourceColumn pos $ sourceColumn pos
matchf :: (Command -> Bool) -> Modes Command
matchf f = Prim.tokenPrim show incCol testChar
    where testChar t = if f t then Just t else Nothing

match c = matchf (==c)

matchcat :: CharCategory -> Modes Command
matchcat cat = matchf testCat
    where
      testCat (CharCommand (TypedChar _ cat')) = cat == cat'
      testCat _ = False
\end{code}

A simple case is \code{charcommand}, which matches any \code{CharCommand}:
\begin{code}
charcommand = matchf (\c -> case c of { CharCommand _ -> True; _ -> False })
\end{code}

These are necessary to read numbers and units from the stream. Error handling
is poor.

\begin{code}
readNumber :: Modes Integer
readNumber = toNumber `liftM` many (matchf isDigit)
    where
        isDigit (CharCommand (TypedChar c Other)) = (c `elem` "0123456789")
        isDigit _ = False
        toNumber :: [Command] -> Integer
        toNumber digits = read $ cvalue `map` digits
            where
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

The implementation of \tex{\\count} is hidden behind a few helper functions:
\begin{code}
getRegisterM :: String -> Integer -> Modes E.HexType
getRegisterM class_ rid = do
    e <- environmentM
    let Just v = E.lookup (class_++"-register:"++show rid) e
    return v

setRegisterM class_ wrapper isglobal rid val =
        modifyState (ins isglobal (class_++"-register"++show rid) (wrapper val))
    where
        ins True = E.globalinsert
        ins False = E.insert

getCountM = liftM (\(E.HexInteger i) -> i) . getRegisterM "count"
setCountM = setRegisterM "count" E.HexInteger

getDimenM = liftM (\(E.HexDimen d) -> d) . getRegisterM "dimen"
setDimenM = setRegisterM "dimen" E.HexDimen

getSkipM = liftM (\(E.HexGlue g) -> g) . getRegisterM "skip"
setSkipM = setRegisterM "skip" E.HexGlue
\end{code}

Now, we come to the actual code for hex. \code{_vModeM} implements v-mode (in
the monad).

\begin{code}
_vModeM :: Modes [VBox]
_vModeM = (eof >> return []) <|> do
    v <- (try vMode1) <|> hMode
    r <- _vModeM
    return (v++r)
\end{code}

\code{vMode1} handles one vertical mode command and drops to hMode if that
fails. Only a few vertical commands are handled currently.
\begin{code}
vMode1 :: Modes [VBox]
vMode1 = anyToken >>= vMode1'
\end{code}

\begin{code}
vMode1' (PrimitiveCommand "\\vspace") = do
    d <- readDimen
    return [Box V d zeroDimen zeroDimen (Kern d)]
\end{code}

\code{setcount} and \code{setdimen} are both very simple:
\begin{code}
vMode1' (SetCountCommand cid val) = do
    setCountM False cid val
    return []

vMode1' (AdvanceCountCommand isg cid val) = do
    v <- getCountM cid
    setCountM isg cid (v + val)
    return []

vMode1' (SetDimenCommand cid val) = do
    setDimenM False cid val
    return []

vMode1' (SetSkipCommand did val) = do
    setSkipM False did val
    return []

\end{code}

\code{outputfont} is needed for internal reasons and causes a font information to be output.
\begin{code}
vMode1' (OutputfontCommand fontinfo) =
    return [Box V zeroDimen zeroDimen zeroDimen (DefineFontContent fontinfo)]
\end{code}

If nothing matches, make the parser fail:
\begin{code}
vMode1' _ = unexpected "not a vmode command"
\end{code}

Now that we have dealt with vertical mode, we must deal with the horizontal.
The first function puts down a single character to form an \code{HElement}:

\begin{code}
setCharacter :: Modes HElement
setCharacter = do
        CharCommand tc <- charcommand
        e <- environmentM
        return $ toHElement e tc
    where
        toHElement e (TypedChar c cat)
                | cat == Space = spaceInFont fnt
                | otherwise = charInFont c fidx fnt
            where (fidx,(_,fnt)) = E.currentfont e
\end{code}

Selecting a font is easy, just set the font in the environment:
\begin{code}
selectfont = do
    SelectfontCommand i fontinfo <- matchf (\t -> case t of { SelectfontCommand _ _ -> True ; _ -> False })
    modifyState (E.setfont i fontinfo)
\end{code}

Setting math fonts is similar
\begin{code}
setmathfont = do
    SetMathFontCommand i fontinfo fam fs <- matchf (\t -> case t of { SetMathFontCommand _ _ _ _ -> True; _ -> False})
    modifyState (\e -> E.setmathfont i fontinfo e fam fs)
\end{code}

A \code{MathCodeCommand} similarly, just modifies the math environment:
\begin{code}
mathcode = do
    MathCodeCommand c mtype fam val <- matchf (\t -> case t of { MathCodeCommand _ _ _ _ -> True; _ -> False})
    modifyState $ (E.insert ("mc-type"++[c]) (E.HexInteger mtype)) .
                (E.insert ("mc-codepoint"++[c]) (E.HexMathCodePoint (val,fam)))
\end{code}

Similarly, a \code{DelCodeCommand} just adds a delcode to the environment as
well:

\begin{code}
delcode = do
    DelCodeCommand c (sval,sfam) (bval,bfam) <- matchf (\t -> case t of { DelCodeCommand _ _ _ -> True; _ -> False})
    modifyState $ (E.insert ("delim-small:" ++ [c]) (E.HexMathCodePoint (sval,sfam))) .
                (E.insert ("delim-big:" ++ [c]) (E.HexMathCodePoint (bval,bfam)))
\end{code}

sfcode is as easy:
\begin{code}
sfcode = do
    SfCodeCommand c sfc <- matchf (\t -> case t of { SfCodeCommand _ _ -> True; _ -> False })
    modifyState (E.insert ("sfcode:" ++ [c]) (E.HexInteger sfc))
\end{code}

Building up, \code{_paragraph} gets a single paragraph as a list of
\code{HElement}s (but they are still just a list). This is a long
case-statement; most of the cases got back to \code{_paragraph} to build a list:
\begin{code}
_paragraph :: Modes [HElement]
_paragraph =
    (eof >> return []) <|>
    (match (PrimitiveCommand "\\par") >> return []) <|>
    (match  PushCommand >> pushE >> _paragraph) <|>
    (match  PopCommand >> popE >> _paragraph) <|>
    (match  MathShiftCommand >> mMode >>= typesetMListM >>= (\ml -> _paragraph >>= return . (ml++))) <|>
    (setCharacter >>= (\h -> _paragraph >>= return . (h:))) <|>
    (mathcode >> _paragraph) <|>
    (delcode >> _paragraph) <|>
    (sfcode >> _paragraph) <|>
    (setmathfont >> _paragraph) <|>
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

Math parsing is implemented here, while \code{Maths} implements math typesetting.
The simplest case is to match a single character as a \code{MChar}:
\begin{code}
singlechar :: Modes MList
singlechar = do
    CharCommand (TypedChar c _) <- charcommand
    e <- environmentM
    let E.HexInteger mtype = E.lookupWithDefault (E.HexInteger 0) ("mc-type"++[c]) e
        E.HexMathCodePoint (val,fam) = E.lookupWithDefault (E.HexMathCodePoint (c,0)) ("mc-codepoint"++[c]) e
    return (case mtype of
        -- 0 ordinary
        0 -> MChar fam val
        -- 1 large
        -- 2 binary
        2 -> MBin (MChar fam val)
        -- 3 relation
        3 -> MRel (MChar fam val)
        -- 4 opening
        -- 5 closing
        -- 6 punctuation
        -- 7 variable
        _ -> MChar fam val
        )
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
    case (up,down) of
        (Nothing, Nothing) -> return c
        _ -> return $ MAtom c up down
mlist = (many matom >>= return . MListList)
\end{code}

End of math mode:

\begin{code}
eomath :: Modes ()
eomath = do
    void $ match MathShiftCommand

mMode :: Modes MList
mMode = pushE *> mlist <* eomath <* popE
\end{code}

We wrap the pure \code{typesetMList} inside a monad interface:
\begin{code}
typesetMListM :: MList -> Modes [HElement]
typesetMListM ml = do
    e <- environmentM
    return $ typesetMList e ml
\end{code}
and we are done with the math mode functionality.

Finally, we hide it all behind a pure interface:
\begin{code}
vMode :: E.Environment String E.HexType -> [Command] -> [VBox]
vMode e cs = case runP _vModeM e "input" cs of
    Right res -> res
    Left err -> error $ show err
\end{code}
