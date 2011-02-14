\section{Streams}
\begin{code}
module CharStream where
\end{code}

Character codes are defined by a table that the user can manipulate. This table
obeys namespace rules. For now, we will just represent the table as a Data.Map:

\begin{code}
import Chars

import qualified Data.Map as Map
type CategoryTable = Map.Map Char CharCategory
\end{code}

This encodes a typed character stream

\begin{code}
data TypedCharStream = TypedCharStream
                { table :: CategoryTable
                , remaining :: [Char]
                } deriving (Eq)
\end{code}


We now define the \code{annotate} function, which is a trivial function. The
reason it takes the table as its \emph{first} argument is that it makes it
easier to curry.

\begin{code}
annotate ::  CategoryTable -> Char -> TypedChar
annotate table c = TypedChar{value=c, category=cat}
    where cat = Map.findWithDefault Other c table
\end{code}

To retrieve a character, the \code{getchar} returns both the character and the
stream. This is a similar interface to the state monad.

\begin{code}
getchar :: TypedCharStream -> (TypedChar, TypedCharStream)
getchar st@TypedCharStream{table=table, remaining=(c:cs)} = (annotate table c, st{remaining=cs})
\end{code}

A simple function to test for an empty stream:

\begin{code}
emptyStream :: TypedCharStream -> Bool
emptyStream st@TypedCharStream{remaining=[]} = True
emptyStream _ = False
\end{code}

