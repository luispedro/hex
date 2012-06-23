\section{Streams}
A character stream is an object which is either empty or can return a typed
char using \code{getchar}. The stream has a list of Char and a conversion table
to transform them into typed chars.

\begin{code}
module CharStream
    ( CategoryTable
    , annotate
    , asqueue
    , _safeget
    , TypedCharStream(..)
    , getchar
    , fnameLine
    , emptyTCS
    , pushst
    , popst
    , prequeue
    , _ncsPrequeue
    ) where

import Data.Char
\end{code}

Character codes are defined by a table that the user can manipulate. This table
obeys namespace rules. Therefore, we need to import the \code{Environment}
module.

\begin{code}
import Chars

import qualified Environment as E
import qualified Data.Text.Lazy as LT
import Control.Monad
import Control.Applicative

type CategoryTable = E.Environment Char CharCategory
\end{code}

We now define the \code{annotate} function, which is a trivial function. The
reason it takes the table as its \emph{first} argument is that it makes it
easier to curry.

\begin{code}
annotate ::  CategoryTable -> Char -> TypedChar
annotate tab c = TypedChar{value=c, category=E.lookupWithDefault Other c tab}
\end{code}

This encodes a typed character stream

\begin{code}

data NamedCharStream = EofNCS
    | NamedCharStream
                { ncsData :: LT.Text
                , ncsLine :: !Int
                , ncsFname :: String
                , ncsNext :: NamedCharStream
                }
    deriving (Eq, Show)

asqueue :: String -> LT.Text -> NamedCharStream
asqueue fname cs = NamedCharStream cs 1 fname EofNCS

_safeget :: NamedCharStream -> Maybe (Char,NamedCharStream)
_safeget EofNCS = Nothing
_safeget s@NamedCharStream{ncsData=r, ncsLine=line, ncsNext=next} = case LT.uncons r of
    Nothing -> _safeget next
    Just ('\n',cs) -> Just ('\n',s{ncsData=cs,ncsLine=(1+line)})
    Just (c,cs) -> Just (c,s{ncsData=cs})

gethathat :: NamedCharStream -> Maybe (Char, NamedCharStream)
gethathat s0 = do
    (c1,s1) <- _safeget s0
    guard (c1 == '^')
    (c2,s2) <- _safeget s1
    guard (c2 == '^')
    (v,s3) <- _safeget s2
    let ov = ord v
        c = chr $ ov + (if ov >= 64 then (-64) else 64)
    return (c,s3)

data TypedCharStream = TypedCharStream
                { table :: CategoryTable
                , remaining :: NamedCharStream
                } deriving (Eq, Show)
\end{code}


To retrieve a character, the \code{getchar} returns both the character and the
stream. This is a similar interface to the state monad. If EOF is reached,
returns \code{Nothing}. Up to this level, \code{error} is never called.

\begin{code}
getchar :: TypedCharStream -> Maybe (TypedChar, TypedCharStream)
getchar st@TypedCharStream{table=tab,remaining=q } = (gethathat q <|> _safeget q) >>= (\(c,q') -> Just (annotate tab c, st{remaining=q'}))
\end{code}

This is retrieves the inner positional information:
\begin{code}
fnameLine TypedCharStream{remaining=q} = (ncsFname q, ncsLine q)
\end{code}

An empty \code{TypedCharStream}:
\begin{code}
emptyTCS = TypedCharStream [] EofNCS
\end{code}

To manipulate the stream, we use two functions:

\begin{code}
pushst st@TypedCharStream{table=t} = st{table=E.push t}
popst st@TypedCharStream{table=t} = st{table=E.pop t}
\end{code}

We can pre-queue a string:

\begin{code}
prequeue :: (String,LT.Text) -> TypedCharStream -> TypedCharStream
prequeue input st@TypedCharStream{remaining=q} = st { remaining=_ncsPrequeue input q }

_ncsPrequeue :: (String,LT.Text) -> NamedCharStream -> NamedCharStream
_ncsPrequeue (name,rs) = NamedCharStream rs 0 name
\end{code}


