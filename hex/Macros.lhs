\section{Macros}

Macros are the mechanism for TeX scripting.

\begin{code}
module Macros
    ( expand
    , expandE
    , Lookup(..)
    , Quantity(..)
    , quantity
    , Command(..)
    , HexCommand(..)
    , ExpansionEnvironment(..)
    , gettokenM
    , gettokentilM
    , readNumberM
    , _readUGlueM
    , _breakAtGroupEnd
    ) where

import Data.List (sortBy)
import Data.Char (chr, toUpper, ord)
import Data.Bits
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.RWS.Strict
import qualified Data.AList as AL

import DVI
import Fonts
import Tokens
import FixWords
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
            | UcCode Char
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
        | DeprecatedCommand String
        | InputCommand String
        | MessageCommand String
        | LoadfontHCommand String
        | SelectfontHCommand String
        | SetMathFontHCommand String Integer E.MathFontStyle
        | LookupCountHCommand (Quantity Integer) (Lookup Integer)
        | LookupDimenHCommand (Quantity UDimen) (Lookup Dimen)
        | LookupSkipHCommand (Quantity UGlue) (Lookup Glue)
        | ByeCommand
        deriving (Eq)
\end{code}
We define an extra type so that we can give it a (meaningless) \code{Eq}
instance, so that Command can be in \code{Eq} automatically:

\begin{code}
data Lookup a = Lookup (a -> [Command])

instance Eq (Lookup a) where
    _a == _b = False
\end{code}

Quantities come in three forms: inline (1pt), in registers (\\count1), or in
internal quantities (\\day). We need a type to represent those.
\begin{code}
data Quantity a = QConstant !a
                | QRegister !Integer
                | QInternal !String
                | QScaled !FixWord !(Quantity a)
                deriving (Eq, Show)

quantity :: (Integer -> a) -> (String -> a) -> Quantity a -> a
quantity _ _ (QConstant a) = a
quantity f _ (QRegister ai) = f ai
quantity _ f (QInternal an) = f an
quantity _fi _fs (QScaled _f _q) = error "oops"


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
        | SetCountCommand Bool (Quantity Integer) Integer
        | SetMuGlueCommand Bool (Quantity UGlue) UGlue
        | SetDimenCommand Bool (Quantity UDimen) UDimen
        | SetSkipCommand Bool (Quantity Integer) Glue
        | AdvanceCountCommand Bool (Quantity Integer) (Quantity Integer)
        | AdvanceDimenCommand Bool (Quantity UDimen) (Quantity UDimen)
        | AdvanceSkipCommand Bool (Quantity UGlue) (Quantity UGlue)
        | PrimitiveCommand String
        | InternalCommand ExpansionEnvironment TokenStream HexCommand
        deriving (Eq)


fromToken (ControlSequence csname) = PrimitiveCommand csname
fromToken (CharToken tc) = CharCommand tc

toToken (PrimitiveCommand c) = ControlSequence c
toToken (CharCommand tc) = CharToken tc
toToken _ = error "hex.Macros.toToken: Cannot handle this case"

instance Show HexCommand where
    show (ErrorCommand errmsg) = "error:"++errmsg
    show (DeprecatedCommand errmsg) = "deprecated:"++errmsg
    show (InputCommand fname) = "input:"++fname
    show (MessageCommand msg) = "message:"++msg
    show (LoadfontHCommand fname) = "loadfont:"++fname
    show (SelectfontHCommand fname) = "selectfont:"++fname
    show (SetMathFontHCommand fname _fam _type) = "mathfont:"++fname
    show (LookupCountHCommand cid _) = "lookupcount:"++show cid
    show (LookupDimenHCommand did _) = "lookupdimen:"++show did
    show (LookupSkipHCommand did _) = "lookupskip:"++show did
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
    show (SetCountCommand isg cid val) = concat ["<", if isg then "global " else "", "count ", show cid, " = ", show val, ">"]
    show (SetDimenCommand isg cid val) = concat ["<", if isg then "global " else "", "dimen ", show cid, " = ", show val, ">"]
    show (SetSkipCommand isg cid val) = concat ["<", if isg then "global " else "", "skip ", show cid, " = ", show val, ">"]
    show (SetMuGlueCommand isg mid val) = concat ["<", if isg then "global " else "", "muglue ", show mid, " = ", show val, ">"]
    show (AdvanceCountCommand isg cid val) = concat ["<advance count ", if isg then "global" else "local", " ", show cid, " by ", show val, ">"]
    show (AdvanceDimenCommand isg cid val) = concat ["<advance dimen ", if isg then "global" else "local", " ", show cid, " by ", show val, ">"]
    show (AdvanceSkipCommand isg cid val) = concat ["<advance skip ", if isg then "global" else "local", " ", show cid, " by ", show val, ">"]
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
iparameters =
    ["\\adjdemerits"
    ,"\\binoppenalty"
    ,"\\brokenpenalty"
    ,"\\clubpenalty"
    ,"\\day"
    ,"\\defaulthyphenchar"
    ,"\\defaultskewchar"
    ,"\\delimiterfactor"
    ,"\\displaywidowpenalty"
    ,"\\doublehyphendemerits"
    ,"\\endlinechar"
    ,"\\errorcontextlines"
    ,"\\escapechar"
    ,"\\exhyphenpenalty"
    ,"\\fam"
    ,"\\finalhyphendemerits"
    ,"\\floatingpenalty"
    ,"\\globaldefs"
    ,"\\hangafter"
    ,"\\hbadness"
    ,"\\holdinginserts"
    ,"\\hyphenpenalty"
    ,"\\interlinepenalty"
    ,"\\language"
    ,"\\lefthyphenmin"
    ,"\\linepenalty"
    ,"\\looseness"
    ,"\\mag"
    ,"\\maxdeadcycles"
    ,"\\month"
    ,"\\newlinechar"
    ,"\\outputpenalty"
    ,"\\pausing"
    ,"\\postdisplaypenalty"
    ,"\\predisplaypenalty"
    ,"\\pretolerance"
    ,"\\relpenalty"
    ,"\\righthyphenmin"
    ,"\\showboxbreadth"
    ,"\\showboxdepth"
    ,"\\time"
    ,"\\tolerance"
    ,"\\tracingcommands"
    ,"\\tracinglostchars"
    ,"\\tracingmacros"
    ,"\\tracingonline"
    ,"\\tracingoutput"
    ,"\\tracingpages"
    ,"\\tracingparagraphs"
    ,"\\tracingrestores"
    ,"\\tracingstats"
    ,"\\uchyph"
    ,"\\vbadness"
    ,"\\widowpenalty"
    ,"\\year"
    ]

dparameters =
    ["\\boxmaxdepth"
    ,"\\delimitershortfall"
    ,"\\displayindent"
    ,"\\displaywidth"
    ,"\\emergencystretch"
    ,"\\hangindent"
    ,"\\hfuzz"
    ,"\\hoffset"
    ,"\\hsize"
    ,"\\lineskiplimit"
    ,"\\mathsurround"
    ,"\\maxdepth"
    ,"\\nulldelimiterspace"
    ,"\\overfullrule"
    ,"\\parindent"
    ,"\\predisplaysize"
    ,"\\scriptspace"
    ,"\\splitmaxdepth"
    ,"\\vfuzz"
    ,"\\voffset"
    ,"\\vsize"
    ]

gparameters =
    ["\\abovedisplayskip"
    ,"\\abovedisplayshortskip"
    ,"\\belowdisplayskip"
    ,"\\belowdisplayshortskip"
    ,"\\parskip"
    ,"\\splittopskip"
    ,"\\parfillskip"
    ,"\\topskip"
    ]

mparameters =
    ["\\thickmuskip"
    ,"\\thinmuskip"
    ,"\\medmuskip"
    ]

magic =
    ["error"
    ]
isprimitive csname = (csname `elem` primitives)

isparameter csname = (csname `elem` iparameters)
                || (csname `elem` dparameters)
                || (csname `elem` gparameters)
                || (csname `elem` mparameters)
unexpandable =
    ["\\dimen"
    ,"\\global"
    ,"\\endcsname"
    ]
processed =
    ["\\string"
    ,"\\csname"
    ]
isunexpandable csname = (csname `elem` unexpandable)
                || (csname `elem` magic)
isprocessable csname = (csname `elem` processed)

\end{code}

We need a few helper functions. \code{gettokenorgroup} retrieves the next
\emph{token} or, if it is an enclosed group, it retrieves it as a list.

\begin{code}
gettokenorgroup :: TkSS [Token]
gettokenorgroup = do
    mt <- maybetokenM
    case mt of
        Nothing -> syntaxError "hex.gettokenorgroup end of file" >> return []
        Just t -> if tokenCategory t == BeginGroup
            then breakAtGroupEndM 0
            else return [t]
\end{code}


\code{_breakAtGroupEnd} gets a grouped set of tokens. This function is exported
for testing, but the implementation is based on the monadic
\code{breakAtGroupEndM}:
\begin{code}

_breakAtGroupEnd :: TokenStream -> ([Token], TokenStream)
_breakAtGroupEnd st = let (tks,(_,st'),_) = runTkSS (breakAtGroupEndM 0) undefined st in (tks,st')

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

\begin{code}
syntaxError msg = do
    (fname,line) <- fNamePosM
    context <- ask
    internalCommandM (ErrorCommand $ concat [fname, ":", show line, " syntax error: ", msg, " in context `", context, "`"])
\end{code}

And a little helper function
\begin{code}
syntaxErrorConcat = syntaxError . concat
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
skipifM True = return ()
skipifM False = (void $ gettokentilM isIfEnd) >> skiptokenM
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
expand1 (ControlSequence csname) = local (++" -> expanding `" ++csname++"`") $ do
        e <- envM
        expanded <- case E.lookup csname e of
            Just macro -> expand1' macro
            Nothing -> macronotfounderror csname
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


expand1 t = syntaxErrorConcat ["hex.Macros.expand1: asked to expand non-macro: ", show t]
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
getargs _ = fail "getargs"
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
getCSM :: String -> TkSS String
getCSM errmessage = do
    maybespaceM
    tok <- gettokenM
    case tok of
        ControlSequence c -> return c
        _ -> syntaxError errmessage >> return []
\end{code}

The \code{definemacro} functions defines macros
\begin{code}
definemacro :: Bool -> Bool -> String -> TkSS ()
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
            expandedsubtext = map toToken $ expandE (ExpansionEnvironment env False) $ tokenliststream substitutiontext
            substitution = if edef then expandedsubtext else substitutiontext
        updateEnvM (insertfunction next macro)
    | otherwise = fail "Unexpected control sequence"
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
macronotfounderror csname = do
    (fname,line) <- fNamePosM
    return . errorseq $ concat [fname, ":", show line, " Macro `", csname, "` not defined."]
\end{code}


The main function, \code{expand} is a wrapper around the monadic \code{expandM}:

\begin{code}
expand = expandE (ExpansionEnvironment E.empty False)
expandE :: ExpansionEnvironment -> TokenStream -> [Command]
expandE env st = AL.toList cs
    where
        (_,_,cs) = runTkSS expandM env st
\end{code}


\code{expandM} is actually very simple and just forwards the first token (after
checking that the stream is not empty) to \code{process1}:

\begin{code}
expandM :: TkSS ()
expandM = do
    mt <- maybetokenM
    case mt of
        Nothing -> return ()
        Just t -> (deprecated t) >> (process1 t) >> expandM
\end{code}

To make code simpler, we define a \code{TokenStream} monad, abbreviated TkS:

\begin{code}
type CList = AL.AList Command
type TkS e a = RWS String CList (e,TokenStream) a

tell1 = tell . AL.singleton
bTkS :: (TokenStream -> (a,TokenStream)) -> TkS e a
bTkS f = do
    (e,st) <- get
    let (r,st') = f st
    put (e,st')
    return r
\end{code}

We add several helper function to check the status of the stream and get tokens
(while watching out for eof conditions)

\begin{code}
gettokenM :: TkSS Token
gettokenM = do
    mt <- maybetokenM
    case mt of
        Just t -> return t
        Nothing -> syntaxError "hex.gettokenM: Unexpected EOF" >> return (CharToken (TypedChar '\0' Invalid))

maybetokenM = bTkS $ \ts ->
            case gettoken ts of
                Nothing -> (Nothing, ts)
                Just (t,ts') -> (Just t, ts')
skiptokenM = void maybetokenM

peektokenM = do
    mt <- maybepeektokenM
    case mt of
        Nothing -> syntaxError "hex.peektokenM: Unexpected EOF" >> return (CharToken (TypedChar '\0' Invalid))
        Just t -> return t

maybepeektokenM = bTkS $ \ts ->
        case gettoken ts of
            Nothing -> (Nothing, ts)
            Just (t,_) -> (Just t,ts)
\end{code}

To get a few tokens in a row, we can call \code{gettokentilM}, which returns
all token until a certain condition is fulfilled. The matching token is left in
the queue.
\begin{code}
gettokentilM cond = do
    mc <- maybepeektokenM
    case mc of
        Just tk | not (cond tk) -> do
            void gettokenM
            rest <- gettokentilM cond
            return (tk:rest)
        _ -> return []
\end{code}

Updating the char stream can also be done in the monad:
\begin{code}
updateCharStreamM f = modify (\(e,st) -> (e,updateCharStream st f))
\end{code}

We can add tokens to the start of the queue, either one (\code{puttokenM}) or
several (\code{streamenqueueM}).
\begin{code}
puttokenM t = streamenqueueM [t]
streamenqueueM nts = modify (\(e,st@TokenStream{queue=ts}) -> (e,st{queue=nts ++ ts}))
\end{code}

An often needed operation is to skip an optional space or additional equal
signs. We first implement a generic \code{maybetokM} function, which takes a
condition and skips a token if it matches that condition.
\begin{code}
maybetokM :: (Token -> Bool) -> TkS e ()
maybetokM cond = do
    mt <- maybepeektokenM
    case mt of
        Just t | cond t -> skiptokenM
        _ -> return ()
\end{code}

Now, \code{maybespaceM} and \code{maybeeqM} are simply defined as calls to
\code{maybetokM}:
\begin{code}
maybespaceM = maybetokM ((== Space) . tokenCategory)
maybeeqM = maybetokM (== CharToken (TypedChar '=' Other))
\end{code}


A few primitives (e.g., \tex{\\catcode}) need to read characters:
\begin{code}
readCharM :: TkSS Char
readCharM = do
    skiptokenM
    tk <- gettokenM
    return (case tk of
        CharToken (TypedChar c _) -> c
        ControlSequence ['\\',c] -> c
        _ -> '\0')
\end{code}

Others need to read a string:
\begin{code}
readStrM = do
    t <- maybepeektokenM
    case t of
        Just (CharToken (TypedChar c cat))
            | cat == Space -> return []
            | otherwise -> do
                skiptokenM
                cs <- readStrM
                return (c:cs)
        _ -> return []
\end{code}

Retrieve the current position:
\begin{code}
fNamePosM = bTkS (\tks -> (fnameLine $ charsource tks,tks))
\end{code}
The \code{TkSS} is a token stream and macro environment state monad:
\begin{code}
type TkSS a = TkS ExpansionEnvironment a
runTkSS :: (TkSS a) -> ExpansionEnvironment -> TokenStream -> (a, (ExpansionEnvironment, TokenStream), CList)
runTkSS compute e st = (v,(e', st'),cs)
        where
            (v, (e',st'),cs) = runRWS compute "top" (e, st)

envM :: TkSS MacroEnvironment
envM = gets (definitions . fst)

flagsM :: TkSS Flags
flagsM = gets (flags . fst)

updateEnvM :: (MacroEnvironment -> MacroEnvironment) -> TkSS ()
updateEnvM f = modify (\(ExpansionEnvironment e g,st) -> (ExpansionEnvironment (f e) g, st))
updateFlagsM :: (Flags -> Flags) -> TkSS ()
updateFlagsM f = modify (\(ExpansionEnvironment e g,st) -> (ExpansionEnvironment e (f g), st))
\end{code}

\begin{code}
deprecated (ControlSequence "\\newlinechar") = deprecatedWarning "\\newlinechar is always \\n"
deprecated (ControlSequence "\\pausing") = deprecatedWarning "\\pausing does nothing"
deprecated _ = return ()
deprecatedWarning msg = do
    (fname,line) <- fNamePosM
    context <- ask
    internalCommandM (DeprecatedCommand $ concat [fname, ":", show line, " deprecated command: ", msg, " in context `", context, "`"])
\end{code}

\code{process1} is structured as a huge case statement (implemented with Haskell
pattern matching). It consumes as many tokens as necessary to proces it
sarguments. Some are handled at this level (e.g., \tex{\\let}), while others
geenreate Commands.

\begin{code}
process1 :: Token -> TkSS ()
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
\end{code}

Dealing with \tex{\\noexpand} is easy, just pass the next token unmodified:

\begin{code}
process1 (ControlSequence "\\noexpand") = gettokenM >>= (tell1 . fromToken)
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
    nexte <- case next of
        CharToken _ -> return next
        ControlSequence csname
                | isprimitive csname || isparameter csname || isunexpandable csname -> return next
                | isprocessable csname -> process1 next >> gettokenM
        _ -> expand1 next >> gettokenM
    puttokenM nexte
    puttokenM unexp
\end{code}


The implementation of \tex{\\string} is incomplete as it does not handle the
escapechar setting.
\begin{code}
process1 (ControlSequence "\\string") = do
        unexp <- gettokenM
        case unexp of
            CharToken _ -> puttokenM unexp
            ControlSequence csname -> mapM_ putC (reverse csname)
    where
        putC ' ' = puttokenM (CharToken (TypedChar ' ' Space))
        putC c = puttokenM (CharToken (TypedChar c Other))
\end{code}

\begin{code}
process1 (ControlSequence "\\csname") = do
        tokens <- expandUntil (ControlSequence "\\endcsname")
        case asString tokens of
            Just s -> puttokenM (ControlSequence s)
            Nothing -> syntaxError ("Cannot read a csname correctly")
    where
        asString [] = Just []
        asString (t:ts) = case t of
            CharToken (TypedChar c _) -> asString ts >>= return . (c:)
            _ -> Nothing
        expandUntil lim = do
            t <- expandedTokenM
            if t == lim
                then return []
                else expandUntil lim >>= return . (t:)
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
    tell1 (MathCodeCommand c mtype fam val)
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
    tell1 (DelCodeCommand c (chr' sval,sfam) (chr' bval,bfam))
\end{code}

\begin{code}
process1 (ControlSequence "\\sfcode") = do
    c <- readCharM
    maybeeqM
    n <- readENumberM
    tell1 (SfCodeCommand c n)
\end{code}

\begin{code}
process1 (ControlSequence "\\uccode") = do
    c <- readCharM
    maybeeqM
    uc <- readCharM
    updateEnvM (E.insert ("uccode:"++[c]) (UcCode uc))
\end{code}

\begin{code}
process1 (ControlSequence "\\uppercase") = do
        argument <- gettokenorgroup
        e <- envM
        streamenqueueM (map (uppercaseToken e) argument)
    where
        uppercaseToken e (CharToken (TypedChar c cat)) =
                        let uc = case E.lookup ("uccode:"++[c]) e of
                                Just (UcCode c') -> c'
                                _ -> toUpper c
                        in (CharToken (TypedChar uc cat))
        uppercaseToken _ t = t

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
    let continuation v0 v1 = (skipifM $ evaluatenum v0 (value rel) v1) >> return ()
        continuation0 v = maybeLookup id LookupCountHCommand val1 (continuation v)
    maybeLookup id LookupCountHCommand val0 continuation0
\end{code}

If we run into an \tex{\\else}, then, we were on the true clause of an if and
and need to start skipping (if the file is mal-formed and just contains an
unmatched \tex{\\else}, this becomes \tex{\\iffalse}).

\begin{code}
process1 (ControlSequence "\\else") = skipifM False
\end{code}

If we encounter a \tex{\\fi}, just ignore it.
\begin{code}
process1 (ControlSequence "\\fi") = return ()
\end{code}

The switch statement, \tex{\ifcase}

\begin{code}
process1 (ControlSequence "\\ifcase") = do
        val <- readENumberM
        skipCaseM (val - 1)
    where
        skipCaseM 0 = skipifM True
        skipCaseM n = (void $ gettokentilM isOr) >> skiptokenM >> skipCaseM (n-1)
        isOr (ControlSequence "\\or") = True
        isOr (ControlSequence "\\fi") = True
        isOr _ = False
process1 (ControlSequence "\\or") = skipifM False
\end{code}

If we have a primitive command, then just wrap it up and ship it down.

Defining macros comes in two forms: \tex{\\def} and \tex{\\edef}. The only
difference is whether the \code{substitution} is the code that was presently
directly or its expansion.

\begin{code}
process1 (ControlSequence csname)
    | isprimitive csname = tell1 (PrimitiveCommand csname)
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
        name <- getCSM ("Expected control sequence after "++cdef)
        maybeeqM
        noc <- readENumberOrCountM
        maybeLookup id LookupCountHCommand noc $ \charcode -> do
            updateEnvM (E.insert name (cdefcons charcode))
    where
        cdefcons = if cdef == "\\chardef"
                    then CharDef
                    else MathCharDef
\end{code}

\begin{code}
process1 (ControlSequence cddef)
    | cddef `elem` ["\\countdef", "\\dimendef", "\\skipdef", "\\toksdef"] = do
        name <- getCSM ("Expected a control sequence after "++cddef)
        maybeeqM
        noc <- readENumberOrCountM
        maybeLookup id LookupCountHCommand noc $ \cid -> do
            updateEnvM (E.insert name (c cid))
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
    tell1 (CharCommand (TypedChar (chr $ fromInteger v) Other))
\end{code}

\tex{\\count} is very easy too:
\begin{code}
process1 (ControlSequence "\\count") = do
    cid <- readNumberM
    maybeeqM
    noc <- readENumberOrCountM
    isg <- flagsM
    maybeLookup id LookupCountHCommand noc $ \val ->
        tell1 (SetCountCommand isg (QRegister cid) val)
\end{code}

Integer parameters are handled similar to count registers:
\begin{code}
process1 (ControlSequence csname)
    | csname `elem` iparameters = do
        maybeeqM
        noc <- readENumberOrCountM
        isg <- flagsM
        maybeLookup id LookupCountHCommand noc $ \val ->
            tell1 (SetCountCommand isg (QInternal csname) val)
\end{code}

\tex{\\dimen} is same thing:
\begin{code}
process1 (ControlSequence "\\dimen") = do
    cid <- readNumberM
    maybeeqM
    dim <- readEDimenM
    isg <- flagsM
    lookupDimen dim $ \val ->
        tell1 (SetDimenCommand isg (QRegister cid) (asUDimen val))
\end{code}

\begin{code}
process1 (ControlSequence csname)
    | csname `elem` dparameters = do
        maybeeqM
        dim <- readEDimenM
        isg <- flagsM
        lookupDimen dim $ \val ->
            tell1 (SetDimenCommand isg (QInternal csname) (asUDimen val))
\end{code}

\tex{\\skip} is same thing:
\begin{code}
process1 (ControlSequence "\\skip") = do
    cid <- readNumberM
    maybeeqM
    skip <- _readUGlueM
    isg <- flagsM
    lookupGlue skip $ \g ->
        tell1 (SetSkipCommand isg (QRegister cid) g)

process1 (ControlSequence csname)
    | csname `elem` gparameters = do
        maybeeqM
        skip <- _readUGlueM
        isg <- flagsM
        lookupGlue skip $ \g ->
            tell1 (SetSkipCommand isg (QInternal csname) g)
\end{code}


\begin{code}
process1 (ControlSequence csname)
    | csname `elem` mparameters = do
        maybeeqM
        QConstant val <- _readUGlueM
        isg <- flagsM
        tell1 (SetMuGlueCommand isg (QInternal csname) val)
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
        tell1 (AdvanceCountCommand isg (QRegister count) val)
    where
        maybeToksM = mapM_ maybeTokM
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
                        _ -> countExpected >> return 0
                _ -> countExpected >> return 0
        countExpected = syntaxError "Was expecting a count register"
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
        mfam <- readENumberOrCountM
        maybeLookup id LookupCountHCommand mfam $ \fam -> do
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
            _ -> fail ("hex.Macros.process1: unknown internal command ("++cmdname++")")
    internalCommandM (cmd arg)
\end{code}

The \code{\\input} command has slightly different syntax than most commands,
but again, it is just transformed into an \code{InternalCommand}

\begin{code}
process1 (ControlSequence "\\input") = readStrM >>= (internalCommandM . InputCommand)
\end{code}

\tex{relax} does not do anything, but it is only syntax.
\begin{code}
process1 (ControlSequence "\\relax") = return ()
\end{code}

Finally, we come to the default cases.

\begin{code}
process1 t@(CharToken tc)
    | (category tc) == BeginGroup = (updateEnvM E.push) >> (updateCharStreamM pushst) >> (tell1 PushCommand)
    | (category tc) == EndGroup = do
            e <- envM
            case E.level e of
                1 -> syntaxError "closing environment at top level"
                _ -> (updateEnvM E.pop) >> (updateCharStreamM popst) >> (tell1 PopCommand)
    | (category tc) == MathShift = tell1 MathShiftCommand
    | otherwise = tell1 (fromToken t)
\end{code}

If nothing else triggered, we must have a macro, so we call \code{expand1}. The
expansion will queue the tokens and they will be handled later.

\begin{code}
process1 t = void (expand1 t)
\end{code}

We make it easy to output internal commands:
\begin{code}
internalCommandM c = do
    (e,rest) <- get
    tell1 (InternalCommand e rest c)
\end{code}

We often need to switch levels and ask questions from the next layer.
\begin{code}
lookupDimen :: (Quantity UDimen) -> (Dimen -> TkSS ()) -> TkSS ()
lookupDimen q f = do
    (e,rest) <- get
    let f' = Lookup $ \d ->
                let (_,_,cs) = runTkSS (f d >> expandM) e rest in
                AL.toList cs
    internalCommandM $ LookupDimenHCommand q f'


lookupGlue :: (Quantity UGlue) -> (Glue -> TkSS ()) -> TkSS ()
lookupGlue q f = do
    (e,rest) <- get
    let f' = Lookup $ \g ->
                let (_,_,cs) = runTkSS (f g >> expandM) e rest in
                AL.toList cs
    internalCommandM $ LookupSkipHCommand q f'


maybeLookup :: (a -> b) -> (Quantity a -> Lookup b -> HexCommand) -> Quantity a -> (b -> TkSS ()) -> TkSS ()
maybeLookup t _ (QConstant v) f = f (t v)
maybeLookup _ t q f = do
    (e,rest) <- get
    let f' = Lookup $ \v ->
                let (_,_,cs) = runTkSS (f v >> expandM) e rest in
                AL.toList cs
    internalCommandM $ t q f'
\end{code}

A simple function to read an integer from tokens, which just calls on
\code{readFNumberM} to do all the hard work:
\begin{code}
readNumberM = do
    (x,Nothing) <- readFNumberM False
    return x
\end{code}

This function optionally tries to parse a fraction:
\begin{code}
readFNumberM :: Bool -> TkSS (Integer,Maybe Integer)
readFNumberM checkfraction = local (++" -> readNumberM") $ do
        tk <- peektokenM
        case tk of
            CharToken (TypedChar '"' _) -> skiptokenM >> readNumber ("0x"++) hexdigits >>= noFraction
            CharToken (TypedChar '\'' _) -> skiptokenM >> readNumber ("0o"++) octdigits >>= noFraction
            CharToken (TypedChar '`' _) -> skiptokenM >> gettokenM >>= (\t -> case t of
                            CharToken (TypedChar c _) -> return $! (toInteger . ord $ c, Nothing)
                            ControlSequence ['\\',c] -> return $! (toInteger . ord $ c, Nothing)
                            _ -> syntaxErrorConcat ["Expected character, got ", show t]  >> return (0, Nothing)
                            )
            CharToken (TypedChar c _) | c `elem` decdigits -> readNumber id decdigits >>= maybeFraction checkfraction
            t -> syntaxErrorConcat ["hex.Macros.readNumberM: expected number (got ", show t, ")"] >> return (0,Nothing)
    where
        noFraction x = return (x, Nothing)
        maybeFraction False x = noFraction x
        maybeFraction True x = do
            tk <- maybepeektokenM
            case tk of
                Just (CharToken tc) | value tc == '.' -> do
                    skiptokenM
                    f <- readNumberM
                    return (x, Just f)
                _ -> noFraction x
        readNumber prefix cond = (read . prefix) `liftM` (digits cond)
        digits :: [Char] -> TkS e [Char]
        digits accepted = do
            tok <- maybepeektokenM
            case tok of
                Just (CharToken tc) | ((`elem` accepted) . value) tc -> do
                    skiptokenM
                    rest <- digits accepted
                    return (value tc:rest)
                _ -> return []
        octdigits = "+-01234567"
        decdigits = octdigits ++ "89"
        hexdigits = decdigits ++ "ABCDEF"
\end{code}
This reads expanded tokens to form a number (including interpreting CharDef as
a number, which is what TeX does):

\begin{code}
readENumberOrCountM = local (++" -> readENumberOrCountM") $ do
    t <- gettokenM
    case t of
        CharToken _ -> puttokenM t >> QConstant `liftM` readNumberM
        ControlSequence "\\count" -> QRegister `liftM` readENumberM
        ControlSequence csname | csname `elem` iparameters -> return $! QInternal csname
        ControlSequence csname -> do
            e <- envM
            case E.lookup csname e of
                Just (CharDef v) -> return (QConstant v)
                Just (MathCharDef v) -> return (QConstant v)
                Just (Macro _ _ _ _) -> expand1 t >> readENumberOrCountM
                Just (CountDef cid) -> return (QRegister cid)
                Nothing -> syntaxErrorConcat ["hex.readENumberOrCountM undefined ", csname] >> return (QConstant 0)
                n -> syntaxErrorConcat ["hex.readENumberOrCountM: unexpected: ", show n] >> return (QConstant 0)

readENumberM :: TkSS Integer
readENumberM = local (++" -> readENumberM") $ do
    n <- readENumberOrCountM
    case n of
        QConstant n' -> return n'
        _ -> do
            syntaxError "hex.readENumberM expected number, got count register"
            return 0
\end{code}

Often we need to read an expanded token. This allows mixing between expansion levels:
\begin{code}
expandedTokenM = do
    tk <- gettokenM
    case tk of
        CharToken _ -> return tk
        ControlSequence csname
                | isprimitive csname || isparameter csname || isunexpandable csname -> return tk
                | isprocessable csname -> process1 tk >> expandedTokenM
        _ -> expand1 tk >> expandedTokenM
\end{code}

\code{readEDimenM} reads a dimension in tokensM

\begin{code}
readEDimenM :: TkSS (Quantity UDimen)
readEDimenM = local (++" -> readEDimen") $ do
        tk <- expandedTokenM
        e <- envM
        case tk of
            ControlSequence "\\dimen" -> (QRegister `fmap` readENumberM)
            ControlSequence csname -> case E.lookup csname e of
                Just (DimenDef val) -> return $! QRegister val
                _ -> syntaxErrorConcat ["Expected \\dimen, got ", show tk] >> return (QConstant zeroUDimen)
            _ -> do
                puttokenM tk
                n <- readFNumberM True
                ntk <- expandedTokenM
                puttokenM ntk
                case ntk of
                    ControlSequence _ -> do
                        d <- readEDimenM
                        return $ QScaled (fromPair n) d
                    _ -> do
                        u <- readUnitM
                        return . QConstant $! UDimen (fromInteger . fst $ n) u
    where
        fromPair (int,Nothing) = fromInteger int
        fromPair (int, Just frac) = fromFloat . read $ (show int ++ "." ++ show frac)
        readUnitM = do
            u <- (readSimpleUnit `matchOr` readFill)
            case u of
                Just v -> return v
                Nothing -> (syntaxError "Expected Unit") >> return UnitIn
        readSimpleUnit = do
            tok0 <- expandedTokenM
            tok1 <- expandedTokenM
            case (tok0,tok1) of
                (CharToken c0, CharToken c1)
                    | [value c0, value c1] == "pt" -> return . Just $ UnitPt
                    | [value c0, value c1] == "px" -> return . Just $ UnitPx
                    | [value c0, value c1] == "in" -> return . Just $ UnitIn
                    | [value c0, value c1] == "mu" -> return . Just $ UnitMu
                _ -> return Nothing
        readFill = do
            isfil <- matchETokens "fil"
            if isfil
                then do
                    isfill <- matchETok 'l'
                    if isfill
                        then skiptokenM >> (return . Just $ UnitFill)
                        else return . Just $ UnitFil
                else return Nothing


matchETokens [] = return True
matchETokens (t:ts) = do
    val <- matchETok t
    if val
        then matchETokens ts
        else return False
matchETok t = do
    tk <- expandedTokenM
    case tk of
        CharToken (TypedChar c _) | c == t -> return True
        _ -> puttokenM tk >> return False

_readUGlueM :: TkSS (Quantity UGlue)
_readUGlueM = local (++" -> readENumberM") $ fromJust `fmap` (readSkipReg `matchOr` readUGlueSpec)
    where
        readSkipReg = do
            tk <- expandedTokenM
            e <- envM
            case tk of
                ControlSequence "\\skip" -> (Just . QRegister) `fmap` readENumberM
                ControlSequence csname -> case E.lookup csname e of
                    Just (SkipDef n) -> return . Just . QRegister $ n
                    _ -> return Nothing
                _ -> return Nothing
        readUGlueSpec = do
            QConstant base <- readEDimenM
            maybespaceM
            hplus <- matchETokens "plus"
            maybespaceM
            QConstant st <- if hplus
                            then readEDimenM
                            else (return $ QConstant zeroUDimen)
            maybespaceM
            hminus <- matchETokens "minus"
            maybespaceM
            QConstant sh <- if hminus
                            then readEDimenM
                            else (return $ QConstant zeroUDimen)
            return . Just . QConstant $! UGlue base st sh 0
\end{code}

matchOr is similar to <|>.
\begin{code}
matchOr :: TkSS (Maybe a) -> TkSS (Maybe a) -> TkSS (Maybe a)
matchOr a b = do
    s <- get
    v <- a
    if isJust v
        then return v
        else (put s) >> b
\end{code}
