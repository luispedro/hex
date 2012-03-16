\section{Environment}
\begin{code}
module Environment
    ( HexType(..)
    , BaseEnvironment
    , Environment
    , empty
    , lookup
    , lookupWithDefault
    , push
    , pop
    , insert
    , globalinsert
    , currentfont
    , setfont
    , MathFontStyle(..)
    , mathfont
    , setmathfont
    ) where

import qualified Data.Map as M
import Prelude hiding (lookup)
import Data.Maybe

import Fonts
import DVI (FontDef)
import Measures
\end{code}

This module implements the hex environment, the object which saves what TeX
calls ``parameters'' and macros.

The environment holds a sort of variant type, \code{HexType}:
\begin{code}
data HexType =
        HexDimen Dimen
        | HexInteger Integer
        | HexString String
        | HexFontInfo (FontDef, FontInfo)
        | HexScaledNumber Scaled
\end{code}

An environment is a nested sequence of mappings, which we implement as a list.
The head mapping is the deeper element. As we search for an element, we look
for environments through the list and return the first match found.

\begin{code}
type BaseEnvironment a b = M.Map a b
type Environment a b = [BaseEnvironment a b]
\end{code}

An empty environment is \emph{not} an empty list, but a list with an empty
mapping. The list should never become empty.

\begin{code}
empty :: Environment a b
empty = [M.empty :: (BaseEnvironment a b)]
\end{code}

Lookup is done by looking through the list for the first match.

\begin{code}
lookup :: (Ord a) => a -> Environment a b -> Maybe b
lookup _ [] = Nothing
lookup name (e:es) = case M.lookup name e of
                Just val -> Just val
                Nothing -> lookup name es
lookupWithDefault def name env = fromMaybe def $ lookup name env
\end{code}

Pushing and popping environments are simple list manipulations:

\begin{code}
push = (:) (M.empty :: (BaseEnvironment a b))
pop = tail
\end{code}

There are two types of insertions. Insertion in the current environment and
global insertion. Inserting in the current environment is just a matter of
manipulating the top of the list:

\begin{code}
insert :: (Ord a) => a -> b -> Environment a b -> Environment a b
insert name val (e:es)= (M.insert name val e:es)
insert _ _ [] = error "Inserting on an invalid environment"
\end{code}

Global insertion is achieved by removing the element from all of the
intermediate local environments and inserting it in the last one. See the tests
\code{expanded/global} and \code{expanded/global_intermediate} for use cases.

\begin{code}
globalinsert name val (e:[]) = [M.insert name val e]
globalinsert name val (e:es) = ((M.delete name e):(globalinsert name val es))
globalinsert _ _ [] = error "Inserting on an invalid environment"
\end{code}

We define a few special names by coding them in functions:

\begin{code}
currentfont :: Environment String HexType -> (Integer,(FontDef,FontInfo))
currentfont e = (i,fi)
    where
        Just (HexFontInfo fi) = lookup "currentfont" e
        Just (HexInteger i) = lookup "currentfont-index" e

setfont :: Integer -> (FontDef,FontInfo) -> Environment String HexType -> Environment String HexType
setfont i fi e = e''
    where
        e' = insert "currentfont" (HexFontInfo fi) e
        e'' = insert "currentfont-index" (HexInteger i) e'
\end{code}

\begin{code}
data MathFontStyle = Textfont | Scriptfont | Scriptscriptfont
                    deriving (Eq, Show)

mathfont :: Environment String HexType -> Integer -> MathFontStyle -> (Integer,(FontDef,FontInfo))
mathfont e fam fs = (i,fi)
    where
        Just (HexFontInfo fi) = lookup (concat ["math", code fs, show fam]) e
        Just (HexInteger i) = lookup (concat ["math", code fs, show fam, "-index"]) e

setmathfont :: Integer -> (FontDef,FontInfo) -> Environment String HexType -> Integer -> MathFontStyle -> Environment String HexType
setmathfont i fi e fam fs = e''
    where
        e' = insert (concat ["math", code fs, show fam]) (HexFontInfo fi) e
        e'' = insert (concat ["math", code fs, show fam, "-index"]) (HexInteger i) e'

code Textfont = "textfont"
code Scriptfont = "scriptfont"
code Scriptscriptfont = "scriptscriptfont"

\end{code}
