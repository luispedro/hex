\section{Modes}
This is the high level typesetting interface where \code{Command}s get turned
into boxes.
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

import Text.Parsec hiding (many, anyToken, optional, (<|>))
import qualified Text.Parsec.Prim as Prim
import Control.Monad
import Control.Applicative
\end{code}

The implementation of the modes is as a Parsec based on \code{[Command]}. The
state is the environment:

\begin{code}
type ModeEnvironment= E.Environment String E.HexType
type ModeState = ModeEnvironment
type Modes a = Parsec [Command] ModeState a
\end{code}

Now a few functions to retrieve and manipulate the environment:
\begin{code}
environmentM :: Modes ModeEnvironment
environmentM = getState
\end{code}

Environments are stacks:

\begin{code}
modifyEnvironment :: (ModeEnvironment -> ModeEnvironment) -> Modes ()
modifyEnvironment f = modifyState f
pushE :: Modes ()
pushE = modifyEnvironment E.push
popE :: Modes ()
popE = modifyEnvironment E.pop
\end{code}

Some simple wrappers for boxes:
\begin{code}
insertBox boxn box =
    modifyEnvironment (E.insert ("box:" ++ show boxn) box)
lookupBox boxn = do
    env <- environmentM
    return (E.lookup ("box:" ++ show boxn) env)
clearBox boxn = modifyEnvironment (E.delete ("box:" ++ show boxn))
\end{code}

Small helpers, to match commands that fulfil a certain condition (like
equality) and to match character categories:
\begin{code}
matchf :: (Command -> Bool) -> Modes Command
matchf f = Prim.tokenPrim show incCol testChar
    where
        testChar t = if f t then Just t else Nothing
        incCol pos _ _  = incSourceColumn pos 1

anyCommand :: Modes Command
anyCommand = matchf (const True)

match :: Command -> Modes Command
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
        isDigit (CharCommand (TypedChar c Other)) = c `elem` "0123456789"
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

Now, we come to the actual code for hex. \code{_vModeM} implements v-mode (in
the monad).

\begin{code}
_vModeM :: Modes [VBox]
_vModeM = (eof >> return []) <|> do
    p0 <- getPosition
    v <- try vMode1 <|> hMode
    p1 <- getPosition
    if p0 == p1
        then unexpected (concat ["Infinite loop detected ", show p0, " == ", show p1])
        else do
            r <- _vModeM
            return (v++r)
\end{code}

\code{vMode1} handles one vertical mode command.

\begin{code}
vMode1 :: Modes [VBox]
vMode1 = anyCommand >>= vMode1'
\end{code}

\begin{code}
vMode1' (PrimitiveCommand "\\vspace") = do
    d <- readDimen
    return [Box V d zeroDimen zeroDimen (Kern d)]
\end{code}

\code{outputfont} is needed for internal reasons and causes a font information to be output.
\begin{code}
vMode1' (OutputfontCommand fontinfo) =
    return [Box V zeroDimen zeroDimen zeroDimen (DefineFontContent fontinfo)]
\end{code}

\tex{\setbox} is implemented by type setting the box and then putting it into the environment.
\begin{code}
vMode1' (SetBoxCommand boxn cs) = do
    box <- typesetBox cs
    insertBox boxn box
    return []
\end{code}

Looking up a box is trivial, only complication is the error checking:
\begin{code}
vMode1' (BoxCommand boxn) = do
    maybox <- lookupBox boxn
    case maybox of
        Just (E.HexVBox vb) -> clearBox boxn >> return [vb]
        Nothing -> return []
        _ -> fail ("vMode1: content of \\box[" ++ show boxn ++ "] is not a vbox")
\end{code}

If nothing matches, the parser fails:
\begin{code}
vMode1' c = unexpected (concat ["Expected a vmode command, got ", show c])
\end{code}

Now that we have dealt with vertical mode, we must deal with the horizontal.
The first function puts down a single character to form an \code{HElement}:

\begin{code}
setCharacter :: Modes HElement
setCharacter = do
    CharCommand (TypedChar c cat) <- charcommand
    e <- environmentM
    let (fidx,(_,fnt)) = E.currentfont e
        element = if cat == Space
                    then spaceInFont fnt
                    else charInFont c fidx fnt
        sf = if cat == Space then 0
            else let E.HexInteger i = E.lookupWithDefault (E.HexInteger 0) ("spacefactor:"++[c]) e in i
    modifyEnvironment (E.globalinsert "spacefactor" (E.HexInteger sf))
    return element
\end{code}

Selecting a font is easy, just set the font in the environment:
\begin{code}
selectfont = do
    SelectfontCommand i fontinfo <- matchf (\t -> case t of { SelectfontCommand _ _ -> True ; _ -> False })
    modifyEnvironment (E.setfont i fontinfo)
\end{code}

Setting math fonts is similar
\begin{code}
setmathfont = do
    SetMathFontCommand i fontinfo fam fs <- matchf (\t -> case t of { SetMathFontCommand{} -> True; _ -> False})
    modifyEnvironment (\e -> E.setmathfont i fontinfo e fam fs)
\end{code}

A \code{MathCodeCommand} similarly, just modifies the math environment:
\begin{code}
mathcode = do
    MathCodeCommand c mtype fam val <- matchf (\t -> case t of { MathCodeCommand{} -> True; _ -> False})
    modifyEnvironment $ (E.insert ("mc-type"++[c]) (E.HexInteger mtype)) .
                (E.insert ("mc-codepoint"++[c]) (E.HexMathCodePoint (val,fam)))
\end{code}

Similarly, a \code{DelCodeCommand} just adds a delcode to the environment as
well:

\begin{code}
delcode = do
    DelCodeCommand c (sval,sfam) (bval,bfam) <- matchf (\t -> case t of { DelCodeCommand{} -> True; _ -> False})
    modifyEnvironment $ (E.insert ("delim-small:" ++ [c]) (E.HexMathCodePoint (sval,sfam))) .
                (E.insert ("delim-big:" ++ [c]) (E.HexMathCodePoint (bval,bfam)))
\end{code}

sfcode is as easy:
\begin{code}
sfcode = do
    SfCodeCommand c sfc <- matchf (\t -> case t of { SfCodeCommand _ _ -> True; _ -> False })
    modifyEnvironment (E.insert ("sfcode:" ++ [c]) (E.HexInteger sfc))
\end{code}

Building up, \code{_paragraph} gets a single paragraph as a list of
\code{HElement}s (but they are still just a list, typesetting is done later).

This is a long case-statement; most of the cases got back to \code{_paragraph}
to build a list:

\begin{code}
_paragraph :: Modes [HElement]
_paragraph =
    (eof >> return []) <|>
    hBox <|>
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
    (match (PrimitiveCommand "\\relax") >> _paragraph) <|>
    return []
\end{code}

An \code{hBox} in this context is either directly a \tex{\hbox} or a \tex{\box} which contains an hbox.
\begin{code}
hBox :: Modes [HElement]
hBox = directHBox <|> try hBoxLookup
    where
        directHBox = match (PrimitiveCommand "\\hbox") *> wrapHBox `fmap` _paragraph
        hBoxLookup = anyCommand >>= \c -> case c of
                        BoxCommand boxn -> do
                            maybox <- lookupBox boxn
                            case maybox of
                                Just (E.HexHBox hb) -> clearBox boxn >> return [EBox hb]
                                _ -> fail ("hBoxLookup: content of \\box[" ++ show boxn ++ "] is not an hbox")
                        _ -> fail "not a hbox"
        wrapHBox :: [HElement] -> [HElement]
        wrapHBox boxes = [EBox $ mergeBoxes H (freezehelems boxes)]
\end{code}

Given the list of \code{HElement}s, we need to set them in lines, represented
by \code{VBox}es. \code{typesetParagraph} performs this function:

\begin{code}
typesetParagraph env p = spreadlines baselineskip $ breakintolines linewidth p
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
eomath = void $ match MathShiftCommand

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
vMode :: ModeEnvironment -> [Command] -> [VBox]
vMode e cs = case runP _vModeM e "input" cs of
    Right res -> res
    Left err -> error $ show err
\end{code}

A function to typeset a box so that it can be saved into the environment:

\begin{code}
typesetBox :: [Command] -> Modes E.HexType
typesetBox [] = error "typesetBox should not be called with empty command list"
typesetBox (c:cs) = do
    e <- environmentM
    case c of
        (PrimitiveCommand "\\hbox") -> do
            let r = runP hBox e "typesetBox input" (c:cs)
            case r of
                Right [EBox b] -> return (E.HexHBox b)
                _' -> error ("typesetBox["++show (c:cs)++"]: Should have been two boxes, got {"++show r++"}")
        (PrimitiveCommand "\\vbox") -> do
            let boxed = vMode e cs
            case boxed of
                [b] -> return (E.HexVBox b)
                _ -> error ("typesetBox["++show (c:cs)++"]: Should have been a single box, got {"++show boxed++"}")
        _ -> error "First element of typesetBox should be \\hbox or \\vbox"
\end{code}
