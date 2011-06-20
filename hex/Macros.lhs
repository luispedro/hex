\section{Macros}

Macros are the mechanism for TeX scripting.

\begin{code}
module Macros where

import Tokens
import Chars
import CharStream
import qualified Environment as E
\end{code}

Our implementation of macros is very simple: they are lists of functions. So for example

\\def\\macro\#1\#2\{M\#1CR\#2\}

gets mapped to

[const (TypedChar 'M' Letter), (!! 0), const (TypedChar 'C' Letter), const (TypedChar 'R' Letter), (!! 1)]

which when mapped with the argument list ["A", "O"], leads to MACRO

\begin{code}
data Macro = Macro { nargs :: Int
                   , expansion :: [[[Token]] -> [Token]]
                   }
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

data Command =
        CharCommand TypedChar
        | PrimitiveCommand String
        | InternalCommand MacroEnvironment TokenStream HexCommand

fromToken (ControlSequence seq) = PrimitiveCommand seq
fromToken (CharToken tc) = CharCommand tc

toToken (PrimitiveCommand c) = ControlSequence c
toToken (CharCommand tc) = CharToken tc

instance Show HexCommand where
    show (ErrorCommand errmsg) = "error:"++errmsg
    show (InputCommand fname) = "input:"++fname
    show (MessageCommand msg) = "message:"++msg

instance Show Command where
    show (PrimitiveCommand cmd) = "<" ++ cmd ++ ">"
    show (CharCommand (TypedChar c Letter)) = ['<',c,'>']
    show (CharCommand (TypedChar _ Space)) = "< >"
    show (CharCommand tc) = "<" ++ show tc ++ ">"
    show (InternalCommand _ _ cmd) = "(" ++ show cmd ++ ")"
\end{code}



In order to build an expansion element, we need to map instances of
\tex{\#\emph{n}} to \code{(!! (n-1))} and other tokens to \code{const t}.

\begin{code}
buildExpansion :: [Token] -> [[[Token]] -> [Token]]
buildExpansion [] = []
buildExpansion ((CharToken tc0):(CharToken tc1):ts)
    | (category tc0) == Parameter = (!! ((read [value tc1])- 1)):(buildExpansion ts)
buildExpansion (t:ts) = (const [t]):(buildExpansion ts)
\end{code}

Some control sequences are \emph{primitive}: They should pass by macro
expansion unharmed. For the moment, we use a simple list as implementation, but
hide it behind the \code{isprimitive} function so we can easily change it in
the future.

\begin{code}
primitives :: [String]
primitives =
    ["\\par"
    ,"\\hbox"
    ,"\\vbox"
    ,"\\relax"
    ,"\\bye"
    ]
isprimitive = (`elem` primitives)
\end{code}

We need a few helper functions. \code{gettokenorgroup} retrieves the next
\emph{token} or, if it is an enclosed group, it retrieves it as a list.

\begin{code}
gettokenorgroup :: TokenStream -> ([Token], TokenStream)
gettokenorgroup st | emptyTokenStream st = error "hex.gettokenorgroup end of file"
gettokenorgroup st = let (c,r) = gettoken st in gettokenorgroup' c r
    where
        gettokenorgroup' (CharToken tc) r
            | (category tc) == BeginGroup = breakAtGroupEnd 0 r
        gettokenorgroup' t r = ([t],r)

breakAtGroupEnd :: Integer -> TokenStream -> ([Token], TokenStream)
breakAtGroupEnd _ st
    | emptyTokenStream st = error "hex.breakAtGroupEnd: unexpected end of file."
breakAtGroupEnd n st = breakAtGroupEnd' n t st'
    where
        (t,st') = gettoken st
        breakAtGroupEnd' 0 t@(CharToken tc) rest
            | (category tc) == EndGroup = ([], rest)
            | otherwise = let (tail, r) = breakAtGroupEnd 0 rest in (t:tail, r)

        breakAtGroupEnd' n t rest = (t:tail, r)
            where
                (tail,r) = breakAtGroupEnd n' rest
                n' = case tokenCategory t of
                    BeginGroup -> n + 1
                    EndGroup -> n - 1
                    _ -> n
\end{code}

\begin{code}
tokenCategory (CharToken tc) = category tc
tokenCategory _ = Invalid -- It doesn't matter what
\end{code}

The work horse of this module are the \code{expand1} and \code{expand1'}
functions. When a macro is not found, we insert an \code{error} command into
the stream. It is the downstream responsibility to deal with it.

The first function just attempts to extract the macro

\begin{code}
expand1 :: MacroEnvironment -> TokenStream -> TokenStream
expand1 env st = let (ControlSequence seq, rest) = gettoken st
                    in case E.lookup seq env of
                        Just macro -> expand1' macro rest
                        Nothing -> streamenqueue rest $ macronotfounderror seq
\end{code}

If found, the macro is expanded by \code{expand1'}
\begin{code}
expand1' :: Macro -> TokenStream -> TokenStream
expand1' macro st = streamenqueue rest expanded
    where
        expanded = (concat $ map (\f -> f arguments) (expansion macro))
        (arguments,rest) = getargs (nargs macro) st
        getargs :: Int -> TokenStream -> ([[Token]], TokenStream)
        getargs 0 rest = ([],rest)
        getargs n st = ([a]++as,rest)
            where
                (a,rs) = gettokenorgroup st
                (as,rest) = getargs (n-1) rs
\end{code}

If there is an error, we insert a special token sequence:

\begin{code}
macronotfounderror seq = [(ControlSequence "error"),(CharToken (TypedChar '{' BeginGroup))] ++ errormsg ++ [CharToken (TypedChar '}' EndGroup)]
    where
        errormsg = map (\c -> (CharToken $ TypedChar c Letter)) $ "Macro `" ++ seq ++ "` not defined."
\end{code}

Both \tex{\let} and \tex{\catcode} allow for an optional equals sign after
them:

\begin{code}
optionalequals s = case gettoken s of
    ((CharToken (TypedChar '=' _)),s') -> s'
    _ -> s
\end{code}

The main function, \code{expand} is actually very simple and just forwards to
\code{expand'}:

\begin{code}
expand :: MacroEnvironment -> TokenStream -> [Command]
expand _ st | emptyTokenStream st = []
expand env st = expand' env t rest
    where (t, rest) = gettoken st
\end{code}

\code{expand'} is structured as a huge case statement (implemented with Haskell
pattern matching)

\begin{code}
expand' env (ControlSequence "\\let") st = expand env' rest
    where
        env' = E.insert name macro env
        (ControlSequence name,aftername) = gettoken st
        (replacement,rest) = gettoken $ optionalequals aftername
        macro = case replacement of
                (ControlSequence seq) -> case E.lookup seq env of
                                            Just macro -> macro
                                            Nothing -> Macro 0 [const [replacement]]
                (CharToken tc) -> Macro 0 [const [(CharToken tc)]]
\end{code}

For dealing with \tex{\\noexpand} we add a special case to expand.

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
expand' env t@(ControlSequence "\\expandafter") st = expand env $ streampush rest guard
        where
            (guard, afterguard) = gettoken st
            rest = expand1 env afterguard
\end{code}

Manipulation of catcodes is performed here:

\begin{code}
expand' env (ControlSequence "\\catcode") st = expand env altered
    where
        (t, r0) = readChar $ st
        char = tvalue t
        r1 = optionalequals r0
        (nvalue, r2) = readNumber r1
        altered = updateCharStream r2 $ catcode char nvalue
        catcode c v st@TypedCharStream{table=t} = st{table=(E.insert c (categoryCode v) t)}
        readNumber st = let (ts, r) = gettokentil st (not . (`elem` "0123456789") . tvalue) in (read $ map tvalue ts, r)
        readChar = gettoken . droptoken
        tvalue (CharToken tc) = value tc
        tvalue (ControlSequence ['\\',c]) = c
        tvalue _ = '\0'
\end{code}

Defining macros comes in two forms: \tex{\\def} and \tex{\\edef}. The only
difference is whether the \code{substitution} is the code that was presently
directly or its expansion.

\begin{code}
expand' env (ControlSequence seq) st
    | isprimitive seq = (PrimitiveCommand seq) : (expand env st)
    | seq `elem` ["\\def", "\\gdef", "\\edef", "\\xdef"] = expand env' rest
        where
            edef = seq `elem` ["\\edef", "\\xdef"]
            insertfunction = if seq `elem` ["\\gdef", "\\xdef"] then E.globalinsert else E.insert
            env' = insertfunction name macro env
            macro = Macro ((`div` 2) (length args)) $ buildExpansion substitution
            (ControlSequence name,aftername) = gettoken st
            (args,afterargs) = gettokentil aftername isBeginGroup
            (substitutiontext,rest) = breakAtGroupEnd 0 $ droptoken afterargs
            substitution = if edef then expandedsubtext else substitutiontext
            expandedsubtext = map toToken $ expand env $ tokenliststream substitutiontext
            isBeginGroup (CharToken tc) = (category tc) == BeginGroup
            isBeginGroup _ = False
\end{code}

We handle \code{\\global} by simply transforming it into \code{\\gdef} or
\code{\\xdef}.

\begin{code}
expand' env (ControlSequence "\\global") st
    | next == "\\def" = expand' env (ControlSequence "\\gdef") rest
    | next == "\\edef" = expand' env (ControlSequence "\\xdef") rest
    where (ControlSequence next, rest) = gettoken st
\end{code}

We need to special case the internal commands. Errors and messages are similar
and handled by the same function:

\begin{code}
expand' env (ControlSequence cs) st
    | cs `elem` ["error","\\message"] = (InternalCommand env rest $ cmd arg):(expand env rest)
    where
        cmd = if cs == "error" then ErrorCommand else MessageCommand
        (argtoks, rest) = gettokenorgroup st
        arg = map (\t -> case t of (CharToken (TypedChar c _)) -> c) argtoks
\end{code}

The \code{\\input} command has slightly different syntax than most commands:

\begin{code}
expand' env (ControlSequence "\\input") st = [InternalCommand env rest $ InputCommand fname]
    where
        (fname, rest) = getletterseq st
        getletterseq st
            | emptyTokenStream st = ([], st)
            | otherwise = case tok of
                (ControlSequence _) -> ([], st)
                (CharToken tc) ->
                    if (category tc) == Letter then
                        let (e,r) = getletterseq rest in (((value tc):e),r)
                        else ([], st)
            where (tok,rest) = gettoken st
\end{code}

\begin{code}
expand' env t@(CharToken tc) st
    | (category tc) == BeginGroup = (fromToken t):(expand (E.push env) (updateCharStream st pushst))
    | (category tc) == EndGroup = (fromToken t):(expand (E.pop env) (updateCharStream st popst))
    | otherwise = (fromToken t):(expand env st)
expand' env t st = expand env $ expand1 env $ streampush st t
\end{code}

\begin{code}
emptyenv = E.empty
\end{code}
