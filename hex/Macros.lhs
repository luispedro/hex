\section{Macros}

Macros are the mechanism for TeX scripting.

\begin{code}
module Macros where

import qualified Data.Map as Map

import Tokens
import Chars
import CharStream

\end{code}

After expansion, we no longer have tokens: we have commands. For the moment, we
have only very simple commands:

\begin{code}
data Command = CharCommand TypedChar | PrimitiveCommand String

fromToken (ControlSequence seq) = PrimitiveCommand seq
fromToken (CharToken tc) = CharCommand tc

toToken (PrimitiveCommand c) = ControlSequence c
toToken (CharCommand tc) = CharToken tc

instance Show Command where
    show (PrimitiveCommand cmd) = "<\\" ++ cmd ++ ">"
    show (CharCommand (TypedChar c Letter)) = ['<',c,'>']
    show (CharCommand (TypedChar _ Space)) = "< >"
    show (CharCommand tc) = "<" ++ show tc ++ ">"
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

In order to build an expansion element, we need to map instances of
\tex{\#\emph{n}} to \code{(!! (n-1))} and other tokens to \code{const t}.

\begin{code}
buildExpansion :: [Token] -> [[[Token]] -> [Token]]
buildExpansion [] = []
buildExpansion ((CharToken tc0):(CharToken tc1):ts)
    | (category tc0) == Parameter = (!! ((read [value tc1])- 1)):(buildExpansion ts)
buildExpansion (t:ts) = (const [t]):(buildExpansion ts)
\end{code}
\begin{code}
type Environment = Map.Map String Macro
\end{code}

Some control sequences are \emph{primitive}: They should pass by macro
expansion unharmed. For the moment, we use a simple list as implementation, but
hide it behind the \code{isprimitive} function so we can easily change it in
the future.

\begin{code}
primitives :: [String]
primitives =
    ["par"
    ,"hbox"
    ,"vbox"
    ,"relax"
    ,"bye"
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
                tokenCategory (CharToken tc) = category tc
                tokenCategory _ = Invalid -- It doesn't matter what
\end{code}

The work horse of this module is the \code{expand1} function.

\begin{code}
expand1 :: Environment -> TokenStream -> TokenStream
expand1 env st = expand1' t r
    where
        (t,r) = gettoken st
        expand1' (ControlSequence seq) r = streamenqueue rest expanded
            where
                expanded = (concat $ map (\f -> f arguments) (expansion macro))
                Just macro = Map.lookup seq env
                (arguments,rest) = getargs (nargs macro) r
                getargs :: Int -> TokenStream -> ([[Token]], TokenStream)
                getargs 0 rest = ([],rest)
                getargs n st = ([a]++as,rest)
                    where
                        (a,rs) = gettokenorgroup st
                        (as,rest) = getargs (n-1) rs
        expand' _ _ = st
\end{code}
\begin{code}
expand :: Environment -> TokenStream -> [Command]
expand _ st | emptyTokenStream st = []
expand env st = expand' env t rest
    where (t, rest) = gettoken st
\end{code}


\begin{code}
expand' env (ControlSequence "let") st = expand env' rest
    where
        env' = Map.insert name macro env
        (ControlSequence name,aftername) = gettoken st
        (replacement,rest) = case gettoken aftername of
                        ((CharToken (TypedChar '=' _)),r) -> gettoken r
                        (t,r) -> (t,r)
        macro = case replacement of
                (ControlSequence seq) -> case Map.lookup seq env of Just macro -> macro
                (CharToken tc) -> Macro 0 [const [(CharToken tc)]]
\end{code}

For dealing with \tex{\\noexpand} we add a special case to expand.

\begin{code}
expand' env (ControlSequence "noexpand") st = (fromToken t:expand env r)
    where (t,r) = gettoken st
\end{code}

\code{expandafter} is dealt in a nice way. Note that, at least in TeX, the
following leads to an error:

\begin{tex}
\expandafter\a\def\a\{Ana\}
\end{tex}

Therefore, the environment cannot change in the inner expansion.
\begin{code}
expand' env t@(ControlSequence "expandafter") st = expand env $ streampush rest guard
        where
            (guard, afterguard) = gettoken st
            rest = expand1 env afterguard
\end{code}

Manipulation of catcodes is performed here:

\begin{code}
expand' env (ControlSequence "catcode") st = expand env altered
    where
        (t, r0) = readChar st
        char = tvalue t
        (eq,r1) = gettoken r0
        (nvalue, r2) = readNumber r1
        altered = updateCharStream r2 $ catcode char nvalue
        catcode c v st@TypedCharStream{table=t} = st{table=(Map.insert c (categoryCode v) t)}
        readNumber st = let (ts, r) = gettokentil st (not . (`elem` "0123456789") . tvalue) in (read $ map tvalue ts, r)
        readChar = gettoken . droptoken
        tvalue (CharToken tc) = value tc
        tvalue (ControlSequence [c]) = c
        tvalue _ = '\0'
\end{code}

Defining macros comes in two forms: \tex{\\def} and \tex{\\edef}. The only
difference is whether the \code{substitution} is the code that was presently
directly or its expansion.

\begin{code}
expand' env (ControlSequence seq) st
    | isprimitive seq = (PrimitiveCommand seq) : (expand env st)
    | (seq == "def") || (seq == "edef") = expand env' rest
        where
            env' = Map.insert name macro env
            macro = Macro ((`div` 2) (length args)) $ buildExpansion substitution
            (ControlSequence name,aftername) = gettoken st
            (args,afterargs) = gettokentil aftername isBeginGroup
            (substitutiontext,rest) = breakAtGroupEnd 0 $ droptoken afterargs
            substitution = if seq == "def" then substitutiontext else map toToken $ expand env $ tokenliststream substitutiontext
            isBeginGroup (CharToken tc) = (category tc) == BeginGroup
            isBeginGroup _ = False
\end{code}

\begin{code}
expand' env t@(CharToken _) st = (fromToken t):(expand env st)
expand' env t st = expand env $ expand1 env $ streampush st t

emptyenv :: Environment
emptyenv = Map.empty
\end{code}
