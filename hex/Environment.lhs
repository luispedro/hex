\section{Environment}
\begin{code}
module Environment where

import qualified Data.Map as M
import Prelude hiding (lookup)

import Measures
\end{code}

This module implements the hex environment, the object which saves what TeX
calls ``parameters'' and macros.

The environment holds a sort of variant type, \code{HexType}:
\begin{code}
data HexType =
        HexDimen Dimen
        | HexString String

\end{code}

An environment is a nested sequence of mappings, which we implement as a list.

\begin{code}
type BaseEnvironment = M.Map String HexType
type Environment = [BaseEnvironment]
\end{code}

An empty environment is \emph{not} an empty list, but a list with an empty
mapping:

\begin{code}
empty :: Environment
empty = [M.empty :: BaseEnvironment]
\end{code}

Lookup is done by looking through the list for the first match.

\begin{code}
lookup :: String -> Environment -> Maybe HexType
lookup name [] = Nothing
lookup name (e:es) = case M.lookup name e of
                Just val -> Just val
                Nothing -> lookup name es
\end{code}

Pushing and popping environments are simple list manipulations:

\begin{code}
push = (:) (M.empty :: BaseEnvironment)
pop = tail
\end{code}

There are two types of insertions. Insertion in the current environment and
global insertion. Inserting in the current environment is just a matter of
manipulating the top of the list:

\begin{code}
insert :: String -> HexType -> Environment -> Environment
insert name val (e:es)= (M.insert name val e:es)
\end{code}

Global insertion is insertion at the last element.

\begin{code}
globalinsert name val (e:[]) = [M.insert name val e]
globalinsert name val (e:es) = (e:(globalinsert name val es))
\end{code}

To start with, we have an environment with the textwidth register set. In the
future, this should be done in a hex startup file.

\begin{code}
startenv = globalinsert "textwidth" (HexDimen (dimenFromInches 6)) empty
\end{code}

