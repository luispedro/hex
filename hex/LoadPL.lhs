\begin{code}
module LoadPL where
import List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Char

import String
import Fonts
\end{code}

A property list (PL) file is a collection of LISP-like s-expressions. This file
is equivalent to the TFM file, but in an easier to parse form. Eventually, we
will want to move towards an implementation which parses the TFM file, but for
now, we have a simple (i.e., slow) implementation of s-expression parsing.

\code{getprop} is a helper function to parse an s-expression of the form
\emph{(start nr)}, returning a \code{FixWord}.

\begin{code}
getprop base start input = case find start input of
        Nothing -> FixWord 0.0
        Just n -> FixWord $ (*base) $ read $ takeWhile (/=')') $ drop (n+ (length start)) input
\end{code}

The main function in this module converts a single S-expression to a
\code{GliphMetric} (if one is found; otherwise \code{Nothing}).

\begin{code}
readGliphInformation :: Float -> String -> Maybe GliphMetric
readGliphInformation base input = if not $ isPrefixOf "CHARACTER " input then Nothing else readGliphInformation' base input
readGliphInformation' base input = Just $ GliphMetric c w h d it
    where
        c = if isPrefixOf "CHARACTER C " input
                then input !! (length "CHARACTER C ")
                else chr $ read $ charactercode input
\end{code}
The .pl file has character codes in octal. \code{read} will parse octal if
prefixed with "0o".
\begin{code}
        charactercode = ('0':) . ('o':) . takeWhile (`elem` "0123456789") . drop (length "CHARACTER O ")
        w = getprop base "CHARWD R " input
        h = getprop base "CHARHT R " input
        d = getprop base "CHARDP R " input
        it = getprop base "CHARIC R " input
\end{code}

We need another function that breaks up a long string into many s-expressions.
That is performed by \code{breakIntoSExpressions}. Note that s-expressions are
internally simple \code{String}s.

\begin{code}
breakIntoSExpressions :: String -> [String]
breakIntoSExpressions [] = []
breakIntoSExpressions ('(':rest) = firstS:(breakIntoSExpressions restS)
    where
        (firstS,restS) = parseS 0 rest
        parseS 0 (')':rest) = ([],rest)
        parseS n ('(':rest) = ('(':f,r) where (f,r) = parseS (n+1) rest
        parseS n (')':rest) = (')':f,r) where (f,r) = parseS (n-1) rest
        parseS n (c:cs) = (c:f,r) where (f,r) = parseS n cs
breakIntoSExpressions (c:cs) = breakIntoSExpressions cs
\end{code}

Finally, we need to parse a few relevant pieces of extra information: The
\emph{design size} and the information on the natural size of a space:

\begin{code}
retrieveDesignSize (s:ss) = if isPrefixOf "DESIGNSIZE" s then retrieveDesignSize' s else retrieveDesignSize ss
    where
        retrieveDesignSize' :: String -> Float
        retrieveDesignSize' = read . drop (length "DESIGNSIZE R ")

readSpace base (s:ss) = if isPrefixOf "FONTDIMEN" s then spinfo else readSpace base ss
    where
        spinfo = SpaceInfo w st shr
        w = getprop base "SPACE R " s
        st = getprop base "STRETCH R " s
        shr = getprop base "SHRINK R " s
\end{code}

Now just need to call the \code{readGliphInformation} function for each
S-expression and the functions above for ancilary information.

\begin{code}
loadPL str = FontInfo fi sp
    where
        fi = catMaybes $ map (readGliphInformation designsize) sexprs
        designsize = retrieveDesignSize sexprs
        sp = readSpace designsize sexprs
        sexprs = breakIntoSExpressions str
\end{code}
