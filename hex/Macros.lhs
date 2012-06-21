\section{Macros}

Macros are the mechanism for TeX scripting.

\begin{code}
module Macros
    ( expand
    , Lookup(..)
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
import Measures
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
            | MathCharDef Integer
            | CountDef Integer
            | DimenDef Integer
            | SkipDef Integer
            | ToksDef Integer
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
type Flags = Bool
data ExpansionEnvironment = ExpansionEnvironment
                        { definitions :: MacroEnvironment
                        , flags :: Bool
                        } deriving (Eq, Show)
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
        | LookupCountHCommand Integer (Lookup Integer)
        | ByeCommand
        deriving (Eq)
\end{code}
We define an extra type so that we can give it a (meaningless) \code{Eq}
instance, so that Command can be in \code{Eq} automatically:

\begin{code}
data Lookup a = Lookup (a -> [Command])

instance Eq (Lookup a) where
    _a == _b = False

data Command =
        CharCommand TypedChar
        | PushCommand -- {
        | PopCommand -- }
        | MathShiftCommand
        | OutputfontCommand (FontDef,FontInfo)
        | SelectfontCommand Integer (FontDef,FontInfo)
        | SetMathFontCommand Integer (FontDef,FontInfo) Integer E.MathFontStyle
        | MathCodeCommand Char Integer Integer Char
        | DelCodeCommand Char (Char,Integer) (Char,Integer)
        | SfCodeCommand Char Integer
        | SetCountCommand Integer Integer
        | SetDimenCommand Integer Dimen
        | SetSkipCommand Integer Dimen
        | AdvanceCountCommand Bool Integer (Either Integer Integer)
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
    show (LookupCountHCommand cid _) = "lookupcount:"++show cid
    show ByeCommand = "bye"

instance Show Command where
    show PushCommand = "<{>"
    show PopCommand = "<}>"
    show MathShiftCommand = "<$>"
    show (SelectfontCommand _ _) = "<selectfont>"
    show (SetMathFontCommand _ _ _ _) = "<setmathfontcommand>"
    show (MathCodeCommand c mathtype fam val) = concat ["<mathcode(", [c], "): (", show mathtype, ",", show fam, ", ", [val], ")>"]
    show (DelCodeCommand c (sval,sfam) (bval,bfam)) = concat ["<delcode(", [c], "): (", show sval, ",", show sfam, ", ", show bval, ":", show bfam, ")>"]
    show (SfCodeCommand c sfval) = concat ["<sfcode(", [c], "): (", show sfval, ")>"]
    show (SetCountCommand cid val) = concat ["<count ", show cid, " = ", show val, ">"]
    show (SetDimenCommand cid val) = concat ["<dimen ", show cid, " = ", show val, ">"]
    show (SetSkipCommand cid val) = concat ["<skip ", show cid, " = ", show val, ">"]
    show (AdvanceCountCommand isg cid val) = concat ["<advance ", if isg then "global" else "local", " ", show cid, " by ", show val, ">"]
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


To handle \tex{\\if} statements, we need to evaluate whether they are True or False:
\begin{code}
evaluateif :: String -> TkSS Bool
evaluateif "\\ifx"  = do
        env <- envM
        tok0 <- gettokenM
        tok1 <- gettokenM
        let cond = ifx env tok0 tok1
        return cond
    where
        ifx env (ControlSequence cs0) (ControlSequence cs1) = samemacro env cs0 cs1
        ifx _ _ _ = False
        samemacro env cs0 cs1 = case ((E.lookup cs0 env),(E.lookup cs1 env)) of
            (Nothing, Nothing) -> True
            (Just m0, Just m1) -> (m0 == m1)
            _ -> False
evaluateif "\\if" = do
        cmd0 <- expandedTokenM
        cmd1 <- expandedTokenM
        let res = evalif cmd0 cmd1
        return res
    where
        evalif (CharToken c0) (CharToken c1) = (value c0) == (value c1)
        evalif _ _ = False
evaluateif _  = fail "hex.Macros.evaluateif: Cannot handle this type"

evaluatenum v0 '=' v1 = v0 == v1
evaluatenum v0 '<' v1 = v0 < v1
evaluatenum v0 '>' v1 = v0 > v1
evaluatenum _ _ _ = error "hex.Macros.evaluatenum: Cannot handle this relationship"
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
        expand1' (CharDef cv) = return ((ControlSequence "\\char"):backtotoks cv)
        expand1' (MathCharDef cv) = return ((ControlSequence "\\mathchar"):backtotoks cv)
        expand1' (CountDef cv) = return ((ControlSequence "\\count"):backtotoks cv)
        expand1' (DimenDef cv) = return ((ControlSequence "\\dimen"):backtotoks cv)
        expand1' (SkipDef cv) = return ((ControlSequence "\\skip"):backtotoks cv)
        expand1' (ToksDef cv) = return ((ControlSequence "\\toks"):backtotoks cv)
        expand1' macro = do
                arguments <- getargs (todelims $ arglist macro)
                e <- envM
                -- Error checking:
                --  unless the macro is \long, there should be no \par in the arguments
                --  there should be no \outer macros in the arguments
                -- Currently, if both errors trigger, only one (the \par error) is shown
                let valid_par = (isLong macro) || (noPar `all` arguments)
                    valid_outer = ((noOuter e) `all` arguments)
                    valid = valid_par && valid_outer
                    expanded = expandmacro macro arguments
                    err = (if valid_par then outererror else longerror)
                return (if valid then expanded else err)
            where
                longerror = errorseq "par in non-long macro"
                outererror = errorseq "\\outer macro used as argument"
                noPar (_,tks) = (ControlSequence "\\par") `notElem` tks
                noOuter e (_,tks) = not $ (isOuterMacro e) `any` tks
                isOuterMacro e (ControlSequence cn) = case E.lookup cn e of
                    Just Macro{isOuter=io} -> io
                    _ -> False
                isOuterMacro _ _ = False
        backtotoks cv = (\s -> (CharToken (TypedChar s Other))) `map` (show cv)


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
\end{code}


\begin{code}
getargtil DelimEmpty = maybespaceM >> gettokenorgroup
\end{code}
We cannot simply use \code{gettokentilM} for defining \code{getargtil} because
we need to respect grouping boundaries:
\begin{code}
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

\code{getCSM} gets a control sequence or prints an error:
\begin{code}
getCSM errmessage = do
    tok <- gettokenM
    case tok of
        ControlSequence c -> return c
        _ -> error errmessage
\end{code}

The \code{definemacro} functions defines macros
\begin{code}
definemacro _long outer "\\long" = (getCSM "Expected control sequence after \\long") >>= (definemacro True outer)
definemacro long _outer "\\outer" = (getCSM "Expected control sequence after \\outer") >>= (definemacro long True)

definemacro long outer csname
    | csname `elem` ["\\gdef","\\xdef","\\edef","\\def"] = do
        next <- getCSM "Expected control sequence after \\def"
        globalFlag <- flagsM
        let insertfunction = if
                    globalFlag || (csname `elem` ["\\gdef", "\\xdef"])
                then E.globalinsert
                else E.insert
        env <- envM
        args <- gettokentilM isBeginGroup
        skiptokenM
        substitutiontext <- breakAtGroupEndM 0
        let macro = Macro args substitution outer long
            expandedsubtext = map toToken $ expand env $ tokenliststream substitutiontext
            substitution = if edef then expandedsubtext else substitutiontext
        updateEnvM (insertfunction next macro)
        return Nothing
    | otherwise = error "Unexpected control sequence"
    where
        edef = csname `elem` ["\\edef", "\\xdef"]
        isBeginGroup (CharToken tc) = (category tc) == BeginGroup
        isBeginGroup _ = False
\end{code}

\code{errorseq} is a sequence of tokens which will produce the passed error message:

\begin{code}
errorseq msg =
            [(ControlSequence "error")
            ,(CharToken (TypedChar '{' BeginGroup))
            ] ++ errormsg ++ [
            CharToken (TypedChar '}' EndGroup)]
    where errormsg = map (\c -> (CharToken $ TypedChar c Letter)) $ msg
\end{code}

If we fail to find a macro, we insert a special token sequence:

\begin{code}
macronotfounderror csname = errorseq ("Macro `" ++ csname ++ "` not defined.")
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

The \code{TkSS} is a token stream and macro environment state monad:
\begin{code}
type TkSS a = TkS ExpansionEnvironment a
runTkSS :: (TkSS a) -> MacroEnvironment -> TokenStream -> (a, (MacroEnvironment, TokenStream))
runTkSS compute e st = (v,(definitions me, st'))
        where
            (v, (me,st')) = runState compute (ExpansionEnvironment e False, st)

envM :: TkSS MacroEnvironment
envM = (definitions . fst) `liftM` get

flagsM :: TkSS Flags
flagsM = (flags . fst) `liftM` get

updateEnvM :: (MacroEnvironment -> MacroEnvironment) -> TkSS ()
updateEnvM f = modify (\(ExpansionEnvironment e g,st) -> (ExpansionEnvironment (f e) g, st))
updateFlagsM :: (Flags -> Flags) -> TkSS ()
updateFlagsM f = modify (\(ExpansionEnvironment e g,st) -> (ExpansionEnvironment e (f g), st))
\end{code}

\code{process1} is structured as a huge case statement (implemented with Haskell
pattern matching). It consumes as many tokens as necessary to proces it
sarguments. Some are handled at this level (e.g., \tex{\\let}), while others
geenreate Commands.

\begin{code}
process1 :: Token -> TkSS (Maybe Command)
process1 (ControlSequence "\\let") = do
    name <- getCSM "Control sequence expected after \\let"
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
    nvalue <- readENumberM
    let catcode c v s@TypedCharStream{table=tab} = s{table=(E.insert c (categoryCode v) tab)}
    updateCharStreamM (catcode char nvalue)
    return Nothing
\end{code}

There are a few other "code" commands handled here. They are all similar.
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

\begin{code}
process1 (ControlSequence "\\delcode") = do
    c <- readCharM
    maybeeqM
    n <- readENumberM
    let sfam = (n `shiftR` 16) .&. 0x0f
        sval = (n `shiftR` 12) .&. 0xff
        bfam = (n `shiftR` 8) .&. 0x0f
        bval = n .&. 0xff
        chr' = chr . fromInteger
    return $ Just (DelCodeCommand c (chr' sval,sfam) (chr' bval,bfam))
\end{code}

\begin{code}
process1 (ControlSequence "\\sfcode") = do
    c <- readCharM
    maybeeqM
    n <- readENumberM
    return $ Just (SfCodeCommand c n)
\end{code}

To handle conditionals, \code{evaluateif} is called.
\begin{code}
process1 (ControlSequence csname)
    | csname `elem` ifstarts = do
        cond <- evaluateif csname
        skipifM cond
\end{code}

Ifnum is a special case because we might need to perform a few lookups:
\begin{code}
process1 (ControlSequence "\\ifnum") = do
    val0 <- readENumberOrCountM
    rel' <- expandedTokenM
    let rel = case rel' of
            CharToken r -> r
            t -> error $ concat ["hex.process1(ifnum): expected relationship character, got ", show t]
    val1 <- readENumberOrCountM
    let continuation v0 v1 = (skipifM $ evaluatenum v0 (value rel) v1) >> return Nothing
        mret (Left v) f = f v
        mret (Right cid) f = do
            f' <- buildLookup f
            internalCommandM $ LookupCountHCommand cid f'
        continuation0 v = mret val1 (continuation v)
    mret val0 continuation0
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

If we have a primitive command, then just wrap it up and ship it down.

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
    updateFlagsM (const True)
    gettokenM >>= process1
\end{code}

\tex{\\chardef} and \tex{\\mathchardef} are almost identical:
\begin{code}
process1 (ControlSequence cdef)
    |cdef `elem` ["\\chardef", "\\mathchardef"] = do
        ControlSequence name <- gettokenM
        maybeeqM
        charcode <- readNumberM
        updateEnvM (E.insert name (cdefcons charcode))
        return Nothing
    where
        cdefcons = if cdef == "\\chardef"
                    then CharDef
                    else MathCharDef
\end{code}

\begin{code}
process1 (ControlSequence cddef)
    | cddef `elem` ["\\countdef", "\\dimendef", "\\skipdef", "\\toksdef"] = do
        ControlSequence name <- gettokenM
        maybeeqM
        cid <- readNumberM
        updateEnvM (E.insert name (c cid))
        return Nothing
    where
        c = case cddef of
                "\\countdef" -> CountDef
                "\\dimendef" -> DimenDef
                "\\skipdef" -> SkipDef
                "\\toksdef" -> ToksDef
                _ -> error "impossible"
\end{code}

\tex{\\char} is very easy:
\begin{code}
process1 (ControlSequence "\\char") = do
    v <- readNumberM
    return $ Just (CharCommand (TypedChar (chr $ fromInteger v) Other))
\end{code}

\tex{\\count} is very easy too:
\begin{code}
process1 (ControlSequence "\\count") = do
    cid <- readNumberM
    maybeeqM
    val <- readNumberM
    return $ Just (SetCountCommand cid val)
\end{code}

\tex{\\dimen} is same thing:
\begin{code}
process1 (ControlSequence "\\dimen") = do
    cid <- readNumberM
    maybeeqM
    val <- readDimenM
    return $ Just (SetDimenCommand cid val)
\end{code}

\tex{\\skip} is same thing:
\begin{code}
process1 (ControlSequence "\\skip") = do
    cid <- readNumberM
    maybeeqM
    val <- readGlueM
    return $ Just (SetSkipCommand cid val)
\end{code}


\begin{code}
process1 (ControlSequence "\\advance") = do
        count <- readCountM
        maybespaceM
        maybeToksM "by"
        maybespaceM
        val <- readENumberOrCountM
        isg <- flagsM
        updateFlagsM (const False)
        return . Just $ AdvanceCountCommand isg count val
    where
        maybeToksM ts = mapM_ maybeTokM ts
        maybeTokM c = do
            tk <- peektokenM
            case tk of
                CharToken (TypedChar c' _) | c == c' -> skiptokenM
                _ -> return ()
        readCountM = do
            t <- gettokenM
            case t of
                ControlSequence "\\count" -> maybespaceM >> readENumberM
                ControlSequence csname -> do
                    e <- envM
                    case E.lookup csname e of
                        Just (CountDef v) -> return v
                        _ -> countExpected
                _ -> countExpected
        countExpected = error "Was expecting a count register"
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

Setting the math fonts is done at another level, so we just collect the
information and pass it down.

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

Errors and messages are similar and handled by the same case (they are similar):
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

If nothing else triggered, we must have a macro, so we call \code{expand1}. The
expansion will queue the tokens and they will be handled later.

\begin{code}
process1 t = (expand1 t) >> (return Nothing)
\end{code}

We make it easy to output internal commands:
\begin{code}
internalCommandM c = do
    (ExpansionEnvironment e _,rest) <- get
    return $ Just (InternalCommand e rest c)
buildLookup :: (a -> TkSS (Maybe Command)) -> TkSS (Lookup a)
buildLookup f = do
    (ExpansionEnvironment e _,rest) <- get
    return . Lookup $ \v ->
                let (mc,(e',r)) = runTkSS (f v) e rest
                    n = expand e' r in
                maybe n (:n) mc
\end{code}

This reads expanded tokens to form a number (including interpreting CharDef as
a number, which is what TeX does):

\begin{code}
readENumberOrCountM = do
    t <- gettokenM
    case t of
        CharToken _ -> puttokenM t >> Left `liftM` readNumberM
        ControlSequence "\\count" -> readENumberM >>= return . Right
        ControlSequence csname -> do
            e <- envM
            case E.lookup csname e of
                Just (CharDef v) -> return (Left v)
                Just (MathCharDef v) -> return (Left v)
                Just (Macro _ _ _ _) -> expand1 t >> readENumberOrCountM
                Just (CountDef cid) -> return (Right cid)
                Nothing -> error $ concat ["hex.readENumberOrCountM undefined ", csname]
                n -> error $ concat ["hex.readENumberOrCountM: unexpected: ", show n]

readENumberM = readENumberOrCountM >>= either return (\_ -> error "hex.readENumberM expected number, got count register")
\end{code}

Often we need to read an expanded token. This allows mixing between expansion levels:
\begin{code}
expandedTokenM = do
    tk <- gettokenM
    case tk of
        (CharToken _) -> return tk
        (ControlSequence _) -> do
            expand1 tk
            gettokenM
\end{code}

\code{readDimenM} reads a dimension in tokens

\begin{code}
readDimenM = do
        n <- readNumberM
        CharToken c0 <- gettokenM
        CharToken c1 <- gettokenM
        return $ dimenFromUnit (fromInteger n) (unit [value c0,value c1])
    where
        unit "pt" = UnitPt
        unit "px" = UnitPx
        unit un = error ("hex.Macros.unit: not implemented: "++un)
readGlueM = readDimenM
\end{code}
