\section{Environment}
\begin{code}
module Environment where

import qualified Data.Map as M
import Prelude hiding (lookup)

import LoadPL (loadPL)
import Fonts
import Measures
\end{code}

This module implements the hex environment, the object which saves what TeX
calls ``parameters'' and macros.

The environment holds a sort of variant type, \code{HexType}:
\begin{code}
data HexType =
        HexDimen Dimen
        | HexString String
        | HexFontInfo FontInfo

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
lookup name [] = Nothing
lookup name (e:es) = case M.lookup name e of
                Just val -> Just val
                Nothing -> lookup name es
lookupWithDefault def name env = case lookup name env of
    Just val -> val
    Nothing -> def
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
\end{code}

Global insertion is achieved by removing the element from all of the
intermediate local environments and inserting it in the last one. See the tests
\code{expanded/global} and \code{expanded/global_intermediate} for use cases.

\begin{code}
globalinsert name val (e:[]) = [M.insert name val e]
globalinsert name val (e:es) = ((M.delete name e):(globalinsert name val es))
\end{code}

We define a few special names by coding them in functions:

\begin{code}
currentFont = lookup "currentfont"
loadfont = (globalinsert "currentfont") . HexFontInfo . loadPL
\end{code}

