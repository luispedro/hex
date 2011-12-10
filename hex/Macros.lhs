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
                } deriving (Eq, Show)
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
        | ByeCommand

data Command =
        CharCommand TypedChar
        | PushCommand -- {
        | PopCommand -- }
        | OutputfontCommand (FontDef,FontInfo)
        | SelectfontCommand Integer (FontDef,FontInfo)
        | PrimitiveCommand String
        | InternalCommand MacroEnvironment TokenStream HexCommand

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
    show ByeCommand = "bye"

instance Show Command where
    show PushCommand = "<{>"
    show PopCommand = "<}>"
    show (SelectfontCommand _ _) = "<selectfont>"
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
gettokenorgroup :: TokenStream -> ([Token], TokenStream)
gettokenorgroup st
    | emptyTokenStream st = error "hex.gettokenorgroup end of file"
gettokenorgroup st = gettokenorgroup' c r
    where
        (c,r) = gettoken st
        gettokenorgroup' (CharToken tc) r'
            | (category tc) == BeginGroup = _breakAtGroupEnd r'
        gettokenorgroup' t r' = ([t],r')
\end{code}


\code{_breakAtGroupEnd} gets a grouped set of tokens.

\begin{code}

_breakAtGroupEnd :: TokenStream -> ([Token], TokenStream)
_breakAtGroupEnd st = runTkS (breakAtGroupEndM 0) st
    where
        breakAtGroupEndM :: Integer -> TkS [Token]
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

Sometimes, the category of a character is important.

\begin{code}
tokenCategory (CharToken tc) = category tc
tokenCategory _ = Invalid -- It doesn't matter what
\end{code}

To handle \tex{\\if} statements, we need to (1) evaluate them and (2) skip over
the non-important characters. The first step is performed by \code{evaluateif}.

\begin{code}
evaluateif :: MacroEnvironment -> String -> TkS (Bool,Int)
evaluateif env "\\ifx"  = do
        tok0 <- gettokenM
        tok1 <- gettokenM
        let cond = ifx tok0 tok1
        return (cond,(if cond then 2 else 0))
    where
        ifx (ControlSequence cs0) (ControlSequence cs1) = samemacro cs0 cs1
        ifx _ _ = False
        samemacro cs0 cs1 = case ((E.lookup cs0 env),(E.lookup cs1 env)) of
            (Nothing, Nothing) -> True
            (Just m0, Just m1) -> (m0 == m1)
            _ -> False
evaluateif env "\\if" = do
        (cmd0:cmd1:_) <- expandM
        let res = evalif cmd0 cmd1
        let skip = (if res then 2 else 0)
        return (res,skip)
    where
        evalif (CharCommand c0) (CharCommand c1) = (value c0) == (value c1)
        evalif _ _ = False
        expandM = TkS $ \st -> (expand env st, undefined)
evaluateif _e _  = fail "hex.Macros.evaluateif: Cannot handle this type"
\end{code}

Skipping depends on the value of the condition. If the condition is true, we do noting. If the condition is false, we skip until the matching \tex{\\else} or
\tex{\\fi}.

\begin{code}
skipif True st = st
skipif False st = droptoken $ snd $ gettokentil st isElseOrFi
    where
        isElseOrFi (ControlSequence c) = c `elem` ["\\else", "\\fi"]
        isElseOrFi _ = False
\end{code}

The work horse of this module are the \code{expand1} and \code{expand1'}
functions. When a macro is not found, we insert an \code{error} command into
the stream. It is the downstream responsibility to deal with it.

The first function just attempts to extract the macro

\begin{code}
expand1 :: MacroEnvironment -> TokenStream -> TokenStream
expand1 env st = case E.lookup csname env of
                    Just macro -> expand1' macro
                    Nothing -> streamenqueue r0 $ macronotfounderror csname
    where
        (ControlSequence csname, r0) = gettoken st
\end{code}
If found, the macro is expanded by \code{expand1'}
\begin{code}
        expand1' :: Macro -> TokenStream
        expand1' macro = streamenqueue r1 expanded
            where
                expanded = expandmacro macro arguments
                arguments :: [(Int,[Token])]
                (arguments,r1) = runTkS (getargs (todelims $ arglist macro)) r0
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
getargs :: [Delim] -> TkS [(Int,[Token])]
getargs (DelimEmpty:_) = return []
getargs (DelimParameter n:d:ds) = do
    maybespaceM
    val <- getargtil d
    rest <- getargs (d:ds)
    return ((n,val):rest)
getargs (DelimToken _:ds) = skiptokenM >> getargs ds
getargs _ = error "getargs"
getargtil DelimEmpty = do
    maybespaceM
    TkS gettokenorgroup
getargtil d@(DelimToken end) = do
    next <- peektokenM
    if (next == end) then
        return []
     else do
        first <- TkS gettokenorgroup
        rest <- getargtil d
        return (first ++ rest)
getargtil (DelimParameter _) = TkS gettokenorgroup
\end{code}


If we fail to find a macro, we insert a special token sequence:

\begin{code}
macronotfounderror csname = [(ControlSequence "error"),(CharToken (TypedChar '{' BeginGroup))] ++ errormsg ++ [CharToken (TypedChar '}' EndGroup)]
    where
        errormsg = map (\c -> (CharToken $ TypedChar c Letter)) $ "Macro `" ++ csname ++ "` not defined."
\end{code}

Both \tex{\let} and \tex{\catcode} allow for an optional equals sign after
them:

\begin{code}
optionalequals s = case gettoken s of
    ((CharToken (TypedChar '=' _)),s') -> s'
    _ -> s
\end{code}

The main function, \code{expand} is actually very simple and just forwards the
first token (after checking that the stream is not empty) to \code{expand'}:

\begin{code}
expand :: MacroEnvironment -> TokenStream -> [Command]
expand _ st | emptyTokenStream st = []
expand env st = expand' env t rest
    where (t, rest) = gettoken st
\end{code}

\code{expand'} is structured as a huge case statement (implemented with Haskell
pattern matching):

\begin{code}
expand' env (ControlSequence "\\let") st = expand env' rest
    where
        env' = E.insert name macro env
        (ControlSequence name,aftername) = gettoken st
        (rep,rest) = gettoken $ optionalequals aftername
        macro = case rep of
                (ControlSequence csname) -> E.lookupWithDefault simple csname env
                (CharToken tc) -> Macro [] [CharToken tc]
        simple = Macro [] [rep]
\end{code}

Dealing with \tex{\\noexpand} is easy, just pass the next token unmodified:

\begin{code}
expand' env (ControlSequence "\\noexpand") st = (fromToken t:expand env r)
    where (t,r) = gettoken st
\end{code}

\code{expandafter} is dealt in a nice way. Note that, at least in TeX, the
following leads to an error:

\begin{tex}
\expandafter\a\def\a\{Anna\}
\end{tex}

Therefore, the environment cannot change in the inner expansion.
\begin{code}
expand' env (ControlSequence "\\expandafter") st = expand env $ streampush rest guard
        where
            (guard, afterguard) = gettoken st
            rest = expand1 env afterguard
\end{code}

Manipulation of catcodes is performed here. It needs to change the token
stream. In hex, the ``environment'' is actually a few separate namespaces,
\tex{\\catcode} only manipulates the category table embedded in the tokens
stream:

\begin{code}
expand' env (ControlSequence "\\catcode") st = expand env altered
    where
        (t, r0) = readChar $ st
        char = tvalue t
        r1 = optionalequals r0
        (nvalue, r2) = readNumber r1
        altered = updateCharStream r2 $ catcode char nvalue
        catcode c v s@TypedCharStream{table=tab} = s{table=(E.insert c (categoryCode v) tab)}
        readNumber st' = let (ts, r) = gettokentil st' (not . (`elem` "0123456789") . tvalue) in (read $ map tvalue ts, r)
        readChar = gettoken . droptoken
        tvalue (CharToken tc) = value tc
        tvalue (ControlSequence ['\\',c]) = c
        tvalue _ = '\0'
\end{code}

To handle conditionals, \code{evaluateif} is called.
\begin{code}
expand' env (ControlSequence csname) st
    | csname `elem` ifstarts = drop toskip $ expand env rest
        where
            ((cond, toskip), _) = runTkS (evaluateif env csname) st
            rest = skipif cond st
\end{code}

If we run into an \tex{\\else}, then, we were on the true clause of an if and
and need to start skipping (if the file is mal-formed and just contains an
unmatched \tex{\\else}, this becomes \tex{\\iffalse}).

\begin{code}
expand' env (ControlSequence "\\else") st = expand env $ skipif False st
\end{code}

If we encounter a \tex{\\fi}, just ignore it.
\begin{code}
expand' env (ControlSequence "\\fi") st = expand env st
\end{code}

Defining macros comes in two forms: \tex{\\def} and \tex{\\edef}. The only
difference is whether the \code{substitution} is the code that was presently
directly or its expansion.

\begin{code}
expand' env (ControlSequence csname) st
    | isprimitive csname = (PrimitiveCommand csname) : (expand env st)
    | csname `elem` ["\\def", "\\gdef", "\\edef", "\\xdef"] = expand env' rest
        where
            edef = csname `elem` ["\\edef", "\\xdef"]
            insertfunction = if csname `elem` ["\\gdef", "\\xdef"] then E.globalinsert else E.insert
            env' = insertfunction name macro env
            macro = Macro args substitution
            (ControlSequence name,aftername) = gettoken st
            (args,afterargs) = gettokentil aftername isBeginGroup
            (substitutiontext,rest) = _breakAtGroupEnd $ droptoken afterargs
            substitution = if edef then expandedsubtext else substitutiontext
            expandedsubtext = map toToken $ expand env $ tokenliststream substitutiontext
            isBeginGroup (CharToken tc) = (category tc) == BeginGroup
            isBeginGroup _ = False
\end{code}

We handle \code{\\global} by simply transforming it into \code{\\gdef} or
\code{\\xdef}. This will immediately ``goto'' the code above:
\begin{code}
expand' env (ControlSequence "\\global") st
    | next == "\\def" = expand' env (ControlSequence "\\gdef") rest
    | next == "\\edef" = expand' env (ControlSequence "\\xdef") rest
    where (ControlSequence next, rest) = gettoken st
\end{code}

We need to special case the internal commands. The simplest is the \tex{\bye}
command, which speaks for itself:
\begin{code}
expand' env (ControlSequence "\\bye") st = [InternalCommand env st ByeCommand]
\end{code}

Errors and messages are similar and handled by the same case:
\begin{code}
expand' env (ControlSequence cs) st
    | cs `elem` ["error","\\message"] = (InternalCommand env rest $ cmd arg):(expand env rest)
    where
        cmd = if cs == "error" then ErrorCommand else MessageCommand
        (arguments,rest) = runTkS (getargs [DelimParameter 0, DelimEmpty]) st
        [(0,argtoks)] = arguments
        arg = toksToStr argtoks
\end{code}

The \tex{\\hexinternal} is a generic command for accessing hex internals:

\begin{code}
expand' env (ControlSequence "\\hexinternal") st = (InternalCommand env rest $ cmd arg):(expand env rest)
    where
        (arguments,rest) = runTkS (getargs [DelimParameter 0, DelimParameter 1, DelimEmpty]) st
        [(0,cmdtoks),(1,argtoks)] = arguments
        cmdname = toksToStr cmdtoks
        arg = toksToStr argtoks
        cmd = case cmdname of
            "loadfont" -> LoadfontHCommand
            "selectfont" -> SelectfontHCommand
            _ -> error ("hex.Macros.expand': unknown internal command ("++cmdname++")")
\end{code}

The \code{\\input} command has slightly different syntax than most commands,
but again, it is just transformed into an \code{InternalCommand}

\begin{code}
expand' env (ControlSequence "\\input") st = [InternalCommand env rest $ InputCommand fname]
    where
        (fname, rest) = getletterseq st
        getletterseq st'
            | emptyTokenStream st' = ([], st')
            | otherwise = case tok of
                (ControlSequence _) -> ([], st')
                (CharToken tc) ->
                    if (category tc) == Letter then
                        let (e,r) = getletterseq rest' in (((value tc):e),r)
                        else ([], st')
            where (tok,rest') = gettoken st'
\end{code}

Finally, we come to the default cases.

\begin{code}
expand' env t@(CharToken tc) st
    | (category tc) == BeginGroup = PushCommand:(expand (E.push env) (updateCharStream st pushst))
    | (category tc) == EndGroup = PopCommand:(expand (E.pop env) (updateCharStream st popst))
    | otherwise = (fromToken t):(expand env st)
\end{code}

If nothing else triggered, we must have a macro, so we call \code{expand1}:

\begin{code}
expand' env t st = expand env $ expand1 env $ streampush st t
\end{code}

