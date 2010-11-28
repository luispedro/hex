\section{Macros}

Macros are the mechanism for TeX scripting.

\begin{code}
module Macros where

import qualified Data.Map as Map

import Tokens
import Chars
\end{code}

After expansion, we no longer have tokens: we have commands. For the moment, we
have only very simple commands:

\begin{code}
data Command = CharCommand Char | PrimitiveCommand String

fromToken (ControlSequence seq) = PrimitiveCommand seq
fromToken (CharToken tc) = CharCommand $ value tc

instance Show Command where
    show (PrimitiveCommand cmd) = "<\\" ++ cmd ++ ">"
    show (CharCommand c) = ['<',c,'>']
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
    ]
isprimitive = (`elem` primitives)
\end{code}

We need a few helper functions. \code{gettokenorgroup} retrieves the next
\emph{token} or, if it is an enclosed group, it retrieves it as a list.

\begin{code}
gettokenorgroup :: [Token] -> ([Token], [Token])
gettokenorgroup [] = error "hex.gettokenorgroup end of file"
gettokenorgroup ((CharToken tc):ts)
    | (category tc) == BeginGroup = breakAtGroupEnd 0 ts
gettokenorgroup (t:ts) = ([t],ts)

breakAtGroupEnd :: Integer -> [Token] -> ([Token], [Token])
breakAtGroupEnd _ [] = error "hex.breakAtGroupEnd: unexpected end of file."
breakAtGroupEnd 0 ((CharToken tc):ts)
    | (category tc) == EndGroup = ([],ts)
breakAtGroupEnd n (t:ts) = (t:ts',rest)
    where
        (ts',rest) = breakAtGroupEnd n' ts
        n' = case (tokenCategory t) of
            EndGroup -> n - 1
            BeginGroup -> n + 1
            _ -> n
        tokenCategory (CharToken tc) = category tc
        tokenCategory _ = Invalid -- It doesn't matter what
\end{code}

The work horse of this module is the \code{expand1} function.

\begin{code}
expand1 :: Environment -> [Token] -> [Token]
expand1 env ((ControlSequence seq):ts) = (concat $ map (\f -> f arguments) (expansion macro)) ++ rest
    where
        Just macro = Map.lookup seq env
        (arguments,rest) = getargs (nargs macro) ts
        getargs :: Int -> [Token] -> ([[Token]], [Token])
        getargs 0 rest = ([],rest)
        getargs n ts = ([a]++as,rest)
            where
                (a,rs) = gettokenorgroup ts
                (as,rest) = getargs (n-1) rs
expand1 env ts = ts
\end{code}
\begin{code}
expand :: Environment -> [Token] -> [Command]
expand _ [] = []
expand env (t@(ControlSequence seq):ts)
    | isprimitive seq = (PrimitiveCommand seq) : (expand env ts)
    | (seq == "def") = expand env' rest
        where
            env' = Map.insert name macro env
            macro = Macro ((`div` 2) (length args)) $ buildExpansion substitution
            ControlSequence name = head ts
            (args,afterargs) = substtextstart $ tail ts
            (substitution,rest) = breakAtGroupEnd 0 $ tail afterargs
            substtextstart = break isBeginGroup
            isBeginGroup (CharToken tc) = (category tc) == BeginGroup
            isBeginGroup _ = False
\end{code}

\code{expandafter} is dealt in a nice way. Note that, at least in TeX, the
following leads to an error:

\begin{tex}
\expandafter\a\def\a\{Ana\}
\end{tex}

Therefore, the environment cannot change in the inner expansion.

\begin{code}
expand env (t@(ControlSequence seq):ts)
    | (seq == "expandafter") = expand env $ (head ts):(expand1 env $ tail ts)
    | otherwise = expand env $ expand1 env (t:ts)
expand env (t:ts) = (fromToken t):(expand env ts)

plaintexenv :: Environment
plaintexenv = Map.empty
\end{code}
