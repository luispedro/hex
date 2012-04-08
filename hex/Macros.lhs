\section{Macros}

Macros are the mechanism for TeX scripting.

\begin{code}
module Macros
    ( expand
    , Command(..)
    , HexCommand(..)
    , _breakAtGroupEnd
    ) where

import Data.List (sortBy)
import Data.Char (chr)
import Data.Bits
import Control.Monad.State

import DVI
import Fonts
import Tokens
import Chars
import CharStream
import qualified Environment as E
\end{code}

Macros are simple pairs of argument and replacement token strings

\begin{code}
data Macro = Macro
            { arglist :: [Token]
            , replacement :: [Token]
            , isOuter :: Bool
            , isLong :: Bool
            }
            | FontMacro String
            | CharDef Integer
            deriving (Eq, Show)
\end{code}

A small helper function, which reads a number from a token, useful for
processing \tex{#1} and similar token sequences.

\begin{code}
readNumberFromToken :: Token -> Int
readNumberFromToken (CharToken (TypedChar v _)) = read [v]
readNumberFromToken _ = -1
\end{code}

To expand a macro, given a set of arguments, use \code{expandmacro}

\begin{code}
expandmacro :: Macro -> [(Int,[Token])] -> [Token]
expandmacro macro arguments = expandmacro' (replacement macro)
    where
        arguments' = map snd $ sortBy (\(a,_) (b,_) -> (compare a b))  arguments
        expandmacro' [] = []
        expandmacro' ((CharToken (TypedChar _ Parameter)):t@(CharToken c):ts) = case category c of
            Parameter -> (t:expandmacro' ts)
            _ -> (arguments' !! ((readNumberFromToken t) - 1))++(expandmacro' ts)
        expandmacro' (t:ts) = (t:expandmacro' ts)
\end{code}

Now, an environment simply maps macro names (\code{String}s) to \code{Macro}s.

\begin{code}
type MacroEnvironment = E.Environment String Macro
\end{code}

After expansion, we no longer have tokens: we have commands. For the moment, we
have only very simple commands:

\begin{code}
data HexCommand =
        ErrorCommand String
        | InputCommand String
        | MessageCommand String
        | LoadfontHCommand String
        | SelectfontHCommand String
        | SetMathFontHCommand String Integer E.MathFontStyle
        | ByeCommand
        deriving (Eq)

data Command =
        CharCommand TypedChar
        | PushCommand -- {
        | PopCommand -- }
        | MathShiftCommand
        | OutputfontCommand (FontDef,FontInfo)
        | SelectfontCommand Integer (FontDef,FontInfo)
        | SetMathFontCommand Integer (FontDef,FontInfo) Integer E.MathFontStyle
        | MathCodeCommand Char Integer Integer Char
        | PrimitiveCommand String
        | InternalCommand MacroEnvironment TokenStream HexCommand
        deriving (Eq)

fromToken (ControlSequence csname) = PrimitiveCommand csname
fromToken (CharToken tc) = CharCommand tc

toToken (PrimitiveCommand c) = ControlSequence c
toToken (CharCommand tc) = CharToken tc
toToken _ = error "hex.Macros.toToken: Cannot handle this case"

instance Show HexCommand where
    show (ErrorCommand errmsg) = "error:"++errmsg
    show (InputCommand fname) = "input:"++fname
    show (MessageCommand msg) = "message:"++msg
    show (LoadfontHCommand fname) = "loadfont:"++fname
    show (SelectfontHCommand fname) = "selectfont:"++fname
    show (SetMathFontHCommand fname _fam _type) = "mathfont:"++fname
    show ByeCommand = "bye"

instance Show Command where
    show PushCommand = "<{>"
    show PopCommand = "<}>"
    show MathShiftCommand = "<$>"
    show (SelectfontCommand _ _) = "<selectfont>"
    show (SetMathFontCommand _ _ _ _) = "<setmathfontcommand>"
    show (MathCodeCommand c mathtype fam val) = concat ["<mathcode(", [c], "): (", show mathtype, ",", show fam, ", ", [val], ")>"]
    show (OutputfontCommand _) = "<outputfont>"
    show (PrimitiveCommand cmd) = "<" ++ cmd ++ ">"
    show (CharCommand (TypedChar c Letter)) = ['<',c,'>']
    show (CharCommand (TypedChar _ Space)) = "< >"
    show (CharCommand tc) = "<" ++ show tc ++ ">"
    show (InternalCommand _ _ cmd) = "(" ++ show cmd ++ ")"
\end{code}

Some control sequences are \emph{primitive}: They should pass by macro
expansion unharmed. For the moment, we use a simple list as implementation, but
hide it behind the \code{isprimitive} function so we can easily change it in
the future.

\begin{code}
ifstarts :: [String]
ifstarts =
    ["\\ifx"
    ,"\\if"
    ]
primitives :: [String]
primitives =
    ["\\par"
    ,"\\hbox"
    ,"\\vbox"
    ,"\\relax"
    ,"\\hspace"
    ,"\\vspace"
    ]
isprimitive = (`elem` primitives)
\end{code}

We need a few helper functions. \code{gettokenorgroup} retrieves the next
\emph{token} or, if it is an enclosed group, it retrieves it as a list.

\begin{code}
gettokenorgroup :: TkSS [Token]
gettokenorgroup = do
    mt <- maybetokenM
    case mt of
        Nothing -> fail "hex.gettokenorgroup end of file"
        Just t -> if tokenCategory t == BeginGroup
            then breakAtGroupEndM 0
            else return [t]
\end{code}


\code{_breakAtGroupEnd} gets a grouped set of tokens. This function is exported
for testing, but the implementation is based on the monadic
\code{breakAtGroupEndM}:
\begin{code}

_breakAtGroupEnd :: TokenStream -> ([Token], TokenStream)
_breakAtGroupEnd st = let (tks,(_,st')) = runTkSS (breakAtGroupEndM 0) undefined st in (tks,st')

breakAtGroupEndM :: Integer -> TkSS [Token]
breakAtGroupEndM n = do
    tk <- gettokenM
    if (n == 0) && (tokenCategory tk) == EndGroup then
        return []
     else do
        rest <- breakAtGroupEndM (n' $ tokenCategory tk)
        return (tk:rest)
    where
        n' BeginGroup = n + 1
        n' EndGroup = n - 1
        n' _ = n
\end{code}


To handle \tex{\\if} statements, we need to (1) evaluate them and (2) skip over
the non-important characters. The first step is performed by \code{evaluateif}.

\begin{code}
evaluateif :: MacroEnvironment -> String -> TkSS Bool
evaluateif env "\\ifx"  = do
        tok0 <- gettokenM
        tok1 <- gettokenM
        let cond = ifx tok0 tok1
        return cond
    where
        ifx (ControlSequence cs0) (ControlSequence cs1) = samemacro cs0 cs1
        ifx _ _ = False
        samemacro cs0 cs1 = case ((E.lookup cs0 env),(E.lookup cs1 env)) of
            (Nothing, Nothing) -> True
            (Just m0, Just m1) -> (m0 == m1)
            _ -> False
evaluateif _ "\\if" = do
        cmd0 <- expandedTokenM
        cmd1 <- expandedTokenM
        let res = evalif cmd0 cmd1
        return res
    where
        evalif (CharToken c0) (CharToken c1) = (value c0) == (value c1)
        evalif _ _ = False
        expandedTokenM = do
            tk <- gettokenM
            case tk of
                (CharToken _) -> return tk
                (ControlSequence _) -> do
                    expand1 tk
                    gettokenM
evaluateif _e _  = fail "hex.Macros.evaluateif: Cannot handle this type"
\end{code}

Skipping depends on the value of the condition. If the condition is true, we do
noting. If the condition is false, we skip until the matching \tex{\\else} or
\tex{\\fi}. We always return \code{Nothing} for convenience of use.
\begin{code}
skipifM True = return Nothing
skipifM False = (void $ gettokentilM isIfEnd) >>
                gettokenM >>
                return Nothing
    where
        isIfEnd (ControlSequence c) | (c `elem` ["\\else", "\\fi"]) = True
        isIfEnd _ = False
\end{code}

The work horse of this module are the \code{expand1} and \code{expand1'}
functions. When a macro is not found, we insert an \code{error} command into
the stream. It is the downstream responsibility to deal with it.

The first function just attempts to extract the macro

\begin{code}
expand1 :: Token -> TkSS ()
expand1 (ControlSequence csname) = do
        e <- envM
        expanded <- (case E.lookup csname e of
            Just macro -> expand1' macro
            Nothing -> return (macronotfounderror csname))
        streamenqueueM expanded
    where
\end{code}
If found, the macro is expanded by \code{expand1'}
\begin{code}
        expand1' :: Macro -> TkSS [Token]
        expand1' (FontMacro fname) = return ((ControlSequence "\\hexinternal"):map toTok ("{selectfont}{" ++ fname ++ "}"))
            where
                toTok '{' = (CharToken (TypedChar '{' BeginGroup))
                toTok '}' = (CharToken (TypedChar '}' EndGroup))
                toTok c = (CharToken (TypedChar c Letter))
        expand1' (CharDef cv) = return ((ControlSequence "\\char"):backtotoks)
            where
                backtotoks = (\s -> (CharToken (TypedChar s Other))) `map` (show cv)
        expand1' macro = do
                arguments <- getargs (todelims $ arglist macro)
                let valid = (isLong macro) || (noPar `all` arguments)
                    expanded = expandmacro macro arguments
                return (if valid then expanded else longerror)
            where
                longerror = errorseq "par in non-long macro"
                noPar (_,tks) = (ControlSequence "\\par") `notElem` tks

expand1 t = error $ concat ["hex.Macros.expand1: asked to expand non-macro: ", show t]
\end{code}

Matching macro parameters is done by attempting to match delimiters. A special
delimiter \code{DelimEmpty} matches the empty string.

\begin{code}
data Delim =
    DelimParameter Int -- #n
    | DelimToken Token -- something like \a or a
    | DelimEmpty -- \epsilon at end of string
    deriving(Show)
todelims [] = [DelimEmpty]
todelims (t:n:ts) | (tokenCategory t) == Parameter = (DelimParameter (readNumberFromToken n)):(todelims ts)
todelims (t:ts) = (DelimToken t):(todelims ts)
\end{code}

\code{getargs} matches a sequence of delimiters to produce the list of
arguments.

\begin{code}
getargs :: [Delim] -> TkSS [(Int,[Token])]
getargs (DelimEmpty:_) = return []
getargs (DelimParameter n:d:ds) = do
    maybespaceM
    val <- getargtil d
    rest <- getargs (d:ds)
    return ((n,val):rest)
getargs (DelimToken _:ds) = skiptokenM >> getargs ds
getargs _ = error "getargs"
getargtil DelimEmpty = maybespaceM >> gettokenorgroup
getargtil d@(DelimToken end) = do
    next <- peektokenM
    if (next == end) then
        return []
     else do
        first <- gettokenorgroup
        rest <- getargtil d
        return (first ++ rest)
getargtil (DelimParameter _) = gettokenorgroup
\end{code}

The \code{definemacro} functions defines macros
\begin{code}
definemacro long outer csname = do
        next <- gettokenM
        case csname of
            "\\long" -> definemacro True outer (csnameof next)
            "\\outer" -> definemacro long True (csnameof next)
            _ -> do
                env <- envM
                args <- gettokentilM isBeginGroup
                skiptokenM
                substitutiontext <- breakAtGroupEndM 0
                let macro = Macro args substitution outer long
                    expandedsubtext = map toToken $ expand env $ tokenliststream substitutiontext
                    substitution = if edef then expandedsubtext else substitutiontext
                updateEnvM (insertfunction (csnameof next) macro)
                return Nothing
    where
        edef = csname `elem` ["\\edef", "\\xdef"]
        insertfunction = if csname `elem` ["\\gdef", "\\xdef"] then E.globalinsert else E.insert
        isBeginGroup (CharToken tc) = (category tc) == BeginGroup
        isBeginGroup _ = False
        csnameof (ControlSequence c) = c
        csnameof _ = error "hex.Macros.definemacro: expected control sequence"
\end{code}

If we fail to find a macro, we insert a special token sequence:

\begin{code}
macronotfounderror csname = errorseq ("Macro `" ++ csname ++ "` not defined.")
errorseq msg = [(ControlSequence "error"),(CharToken (TypedChar '{' BeginGroup))] ++ errormsg ++ [CharToken (TypedChar '}' EndGroup)]
    where errormsg = map (\c -> (CharToken $ TypedChar c Letter)) $ msg
\end{code}

The main function, \code{expand} is actually very simple and just forwards the
first token (after checking that the stream is not empty) to \code{process1}:

\begin{code}
expand :: MacroEnvironment -> TokenStream -> [Command]
expand _ st | emptyTokenStream st = []
expand env st = case mc of
        Just c -> c:rest
        Nothing -> rest
    where
        (t, r0) = gettoken st
        (mc,(env',r1)) = runTkSS (process1 t) env r0
        rest = (expand env' r1)
\end{code}

\code{process1} is structured as a huge case statement (implemented with Haskell
pattern matching):

\begin{code}
type TkSS a = TkS MacroEnvironment a
runTkSS :: (TkSS a) -> MacroEnvironment -> TokenStream -> (a, (MacroEnvironment, TokenStream))
runTkSS compute e st = runState compute (e,st)

envM :: TkSS MacroEnvironment
envM = fst `liftM` get
updateEnvM :: (MacroEnvironment -> MacroEnvironment) -> TkSS ()
updateEnvM f = modify (\(e,st) -> (f e, st))

process1 :: Token -> TkSS (Maybe Command)
process1 (ControlSequence "\\let") = do
    ControlSequence name <- gettokenM
    maybeeqM
    rep <- gettokenM
    env <- envM
    let macro = case rep of
            (ControlSequence csname) -> E.lookupWithDefault simple csname env
            (CharToken tc) -> Macro [] [CharToken tc] False False
        simple = Macro [] [rep] False False
        t = E.insert name macro
    updateEnvM t
    return Nothing
\end{code}

Dealing with \tex{\\noexpand} is easy, just pass the next token unmodified:

\begin{code}
process1 (ControlSequence "\\noexpand") = (Just . fromToken) `liftM` gettokenM
\end{code}

\code{expandafter} is dealt in a nice way. Note that, at least in TeX, the
following leads to an error:

\begin{tex}
\expandafter\a\def\a\{Anna\}
\end{tex}

Therefore, the environment cannot change in the inner expansion.
\begin{code}
process1 (ControlSequence "\\expandafter") = do
    unexp <- gettokenM
    next <- gettokenM
    expand1 next
    (streampushM unexp)
    return Nothing
\end{code}

Manipulation of catcodes is performed here. It needs to change the token
stream. In hex, the ``environment'' is actually a few separate namespaces,
\tex{\\catcode} only manipulates the category table embedded in the tokens
stream:

\begin{code}
process1 (ControlSequence "\\catcode") = do
    char <- readCharM
    maybeeqM
    nvalue <- readNumberM
    let catcode c v s@TypedCharStream{table=tab} = s{table=(E.insert c (categoryCode v) tab)}
    (updateCharStreamM (catcode char nvalue))
    return Nothing
\end{code}

\begin{code}
process1 (ControlSequence "\\mathcode") = do
    c <- readCharM
    maybeeqM
    n <- readNumberM
    let mtype = (n `shiftR` 12) .&. 0x0f
        fam = (n `shiftR` 8) .&. 0x0f
        val = chr $ fromInteger (n .&. 0xff)
    return $ Just (MathCodeCommand c mtype fam val)
\end{code}

To handle conditionals, \code{evaluateif} is called.
\begin{code}
process1 (ControlSequence csname)
    | csname `elem` ifstarts = do
        e <- envM
        cond <- evaluateif e csname
        skipifM cond
\end{code}

If we run into an \tex{\\else}, then, we were on the true clause of an if and
and need to start skipping (if the file is mal-formed and just contains an
unmatched \tex{\\else}, this becomes \tex{\\iffalse}).

\begin{code}
process1 (ControlSequence "\\else") = skipifM False
\end{code}

If we encounter a \tex{\\fi}, just ignore it.
\begin{code}
process1 (ControlSequence "\\fi") = return Nothing
\end{code}

Defining macros comes in two forms: \tex{\\def} and \tex{\\edef}. The only
difference is whether the \code{substitution} is the code that was presently
directly or its expansion.

\begin{code}
process1 (ControlSequence csname)
    | isprimitive csname = return $ Just (PrimitiveCommand csname)
    | csname `elem` ["\\def", "\\gdef", "\\edef", "\\xdef", "\\long", "\\outer"] = definemacro False False csname
\end{code}

We handle \code{\\global} by simply transforming it into \code{\\gdef} or
\code{\\xdef}. This will immediately ``goto'' the code above:
\begin{code}
process1 (ControlSequence "\\global") = do
    next <- gettokenM
    case next of
        ControlSequence "\\def" -> process1 (ControlSequence "\\gdef")
        ControlSequence "\\edef" -> process1 (ControlSequence "\\xdef")
        _ -> error "hex.Macros.process1: Unexpected token after \\global"
\end{code}

\tex{\\chardef} is implemented by transformation into \tex{\\def}:
\begin{code}
process1 (ControlSequence "\\chardef") = do
    ControlSequence name <- gettokenM
    maybeeqM
    charcode <- readNumberM
    updateEnvM (E.insert name (CharDef charcode))
    return Nothing
\end{code}

\begin{code}
process1 (ControlSequence "\\char") = do
    v <- readNumberM
    return $ Just (CharCommand (TypedChar (chr $ fromInteger v) Other))
\end{code}

We need to special case the internal commands. The simplest is the \tex{\bye}
command, which speaks for itself:
\begin{code}
process1 (ControlSequence "\\bye") = internalCommandM ByeCommand
\end{code}

The \tex{\\font} command puts a \code{FontMacro} macro in the environment and
issues a \code{LoadfontHCommand} to load the font. Eagerly loading the font
maps what \TeX{} does, even if a lazy load model could be better (in reality,
the font file is parsed lazily because of Haskell's evaluation model).

\begin{code}
process1 (ControlSequence "\\font") = do
    ControlSequence csname <- gettokenM
    maybeeqM
    fname <- readStrM
    updateEnvM $ E.insert csname (FontMacro fname)
    internalCommandM (LoadfontHCommand fname)
\end{code}

\begin{code}
process1 (ControlSequence cs)
    | cs `elem` ["\\textfont", "\\scriptfont", "\\scriptscriptfont"] = do
        fam <- readNumberM
        maybeeqM
        ControlSequence fc <- gettokenM
        e <- envM
        internalCommandM (case E.lookup fc e of
            Just (FontMacro fname) ->
                SetMathFontHCommand fname fam (fontstyle cs)
            _ -> ErrorCommand $ "Hex: Was expecting a font for \\textfont primitive"
            )
    where
        fontstyle "\\textfont" = E.Textfont
        fontstyle "\\scriptfont" = E.Scriptfont
        fontstyle "\\scriptscriptfont" = E.Scriptscriptfont
        fontstyle _ = error "hex.Macros.process1.fontstyle: This should never happen"
\end{code}

Errors and messages are similar and handled by the same case:
\begin{code}
process1 (ControlSequence cs) | cs `elem` ["error","\\message"] = do
    arguments <- (getargs [DelimParameter 0, DelimEmpty])
    let [(0,argtoks)] = arguments
        arg = toksToStr argtoks
        cmd = if cs == "error" then ErrorCommand else MessageCommand
    internalCommandM (cmd arg)
\end{code}

The \tex{\\hexinternal} is a generic command for accessing hex internals:

\begin{code}
process1 (ControlSequence "\\hexinternal") = do
    arguments <- (getargs [DelimParameter 0, DelimParameter 1, DelimEmpty])
    let [(0,cmdtoks),(1,argtoks)] = arguments
        cmdname = toksToStr cmdtoks
        arg = toksToStr argtoks
        cmd = case cmdname of
            "loadfont" -> LoadfontHCommand
            "selectfont" -> SelectfontHCommand
            _ -> error ("hex.Macros.process1: unknown internal command ("++cmdname++")")
    internalCommandM (cmd arg)
\end{code}

The \code{\\input} command has slightly different syntax than most commands,
but again, it is just transformed into an \code{InternalCommand}

\begin{code}
process1 (ControlSequence "\\input") = readStrM >>= (internalCommandM . InputCommand)
\end{code}

Finally, we come to the default cases.

\begin{code}
process1 t@(CharToken tc)
    | (category tc) == BeginGroup = (updateEnvM E.push) >> (updateCharStreamM pushst) >> (return $ Just PushCommand)
    | (category tc) == EndGroup = (updateEnvM E.pop) >> (updateCharStreamM popst) >> (return $ Just PopCommand)
    | (category tc) == MathShift = return $ Just MathShiftCommand
    | otherwise = return $ Just (fromToken t)
\end{code}

If nothing else triggered, we must have a macro, so we call \code{expand1}:

\begin{code}
process1 t = (expand1 t) >> (return Nothing)
\end{code}

Throughout \code{process1}, we make it easy to output internal commands:
\begin{code}
internalCommandM c = do
    (e,rest) <- get
    return $ Just (InternalCommand e rest c)
\end{code}
