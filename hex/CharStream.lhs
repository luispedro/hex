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

This encodes a typed character stream. We remember the line number and file
name in order to be able to print them out in error messages. Because we can
include files from other files, we have a list of streams: each stream points
to the one to use once the current one becomes empty.

\begin{code}
data NamedCharStream = EofNCS
    | NamedCharStream
                { ncsData :: LT.Text -- ^ input data
                , ncsLine :: !Int -- ^ current line number
                , ncsFname :: String -- ^ current file name
                , ncsNext :: NamedCharStream -- ^ next stream
                }
    deriving (Eq, Show)
\end{code}

A constructor:
\begin{code}
asqueue :: String -> LT.Text -> NamedCharStream
asqueue fname cs = NamedCharStream cs 1 fname EofNCS
\end{code}

\haskell{_safeget} is the raw input function
\begin{code}
_safeget :: NamedCharStream -> Maybe (Char,NamedCharStream)
_safeget EofNCS = Nothing
_safeget s@NamedCharStream{ncsData=r, ncsLine=line, ncsNext=next} = case LT.uncons r of
    Nothing -> _safeget next
    Just ('\n',cs) -> Just ('\n',s{ncsData=cs,ncsLine=(1+line)})
    Just (c,cs) -> Just (c,s{ncsData=cs})
\end{code}

The character stream handles converting \tex{^^ab} encoded characters into real
characters. Note that this is simply a way to input 8-bit characters when the
underlying system does not handle it.

\begin{code}
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
\end{code}

\begin{code}
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
getchar st@TypedCharStream{table=tab,remaining=q } =
                gethathat q <|> _safeget q >>= \(c,q') ->
                    Just (annotate tab c, st{remaining=q'})
\end{code}

This is retrieves the inner positional information:
\begin{code}
fnameLine :: TypedCharStream -> (String, Int)
fnameLine TypedCharStream{remaining=q} = case q of
    EofNCS -> ("EOF [if you see this, this may be a bug in HeX]", -1)
    NamedCharStream{ncsFname=fname, ncsLine=lno} -> (fname, lno)
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


