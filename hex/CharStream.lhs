\section{Streams}
A character stream is an object which is either empty or can return a typed
char using \code{getchar}. The stream has a list of Char and a conversion table
to transform them into typed chars.

\begin{code}
module CharStream
    ( CategoryTable
    , annotate
    , TypedCharStream(..)
    , getchar
    , emptyStream
    , pushst
    , popst
    , prequeue
    ) where
\end{code}

Character codes are defined by a table that the user can manipulate. This table
obeys namespace rules. Therefore, we need to import the \code{Environment}
module.

\begin{code}
import Chars

import qualified Environment as E

type CategoryTable = E.Environment Char CharCategory
\end{code}

We define a few manipulation functions:

We now define the \code{annotate} function, which is a trivial function. The
reason it takes the table as its \emph{first} argument is that it makes it
easier to curry.

\begin{code}
annotate ::  CategoryTable -> Char -> TypedChar
annotate tab c = TypedChar{value=c, category=E.lookupWithDefault Other c tab}
\end{code}

This encodes a typed character stream

\begin{code}
data TypedCharStream = TypedCharStream
                { table :: CategoryTable
                , remaining :: [Char]
                } deriving (Eq)
\end{code}


To retrieve a character, the \code{getchar} returns both the character and the
stream. This is a similar interface to the state monad.

\begin{code}
getchar :: TypedCharStream -> (TypedChar, TypedCharStream)
getchar st@TypedCharStream{table=tab, remaining=(c:cs)} = (annotate tab c, st{remaining=cs})
getchar _ = error "getchar on an empty stream"
\end{code}

A simple function to test for an empty stream:

\begin{code}
emptyStream :: TypedCharStream -> Bool
emptyStream TypedCharStream{remaining=[]} = True
emptyStream _ = False
\end{code}

To manipulate the stream, we use two functions:

\begin{code}
pushst st@TypedCharStream{table=t} = st{table=E.push t}
popst st@TypedCharStream{table=t} = st{table=E.pop t}
\end{code}

We can pre-queue a string:

\begin{code}
prequeue st@TypedCharStream{remaining=q} nq = st{remaining=(nq++q)}
\end{code}


