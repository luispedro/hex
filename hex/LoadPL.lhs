\begin{code}
module LoadPL where
import List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Char

import String
import Fonts

\end{code}

\code{getprop} is a helper function to parse an s-expression of the form
\emph{start nr )}:

\begin{code}
getprop base start input = case find start input of
        Nothing -> FixWord 0.0
        Just n -> FixWord $ (*base) $ read $ takeWhile (/=')') $ drop (n+ (length start)) input
\end{code}

The main function is converting a single S-expression to a \code{GliphMetric}
(if one is found; otherwise \code{Nothing}).

\begin{code}
readGliphInformation :: Float -> String -> Maybe GliphMetric
readGliphInformation base input = if not $ isPrefixOf "CHARACTER " input then Nothing else readGliphInformation' base input
readGliphInformation' base input = Just $ GliphMetric c w h d it
    where
        c = if isPrefixOf "CHARACTER C " input
                then input !! (length "CHARACTER C ")
                else chr $ read $ charactercode input
        -- The .pl file has character codes in octal. `read` will parse octal if prefixed with "0o"
        charactercode = ('0':) . ('o':) . takeWhile (`elem` "0123456789") . drop (length "CHARACTER O ")
        w = getprop base "CHARWD R " input
        h = getprop base "CHARHT R " input
        d = getprop base "CHARDP R " input
        it = getprop base "CHARIC R " input
\end{code}


In order to call that function we need another one that breaks up a long string
into many s-expressions. That is performed by \code{breakIntoSExpressions}.

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

Finally, we need to parse a few relevant extra information: The \emph{design
size} and the information on the space:

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
S-expression.

\begin{code}
loadPL str = FontInfo fi sp
    where
        fi = catMaybes $ map (readGliphInformation designsize) sexprs
        designsize = retrieveDesignSize sexprs
        sp = readSpace designsize sexprs
        sexprs = breakIntoSExpressions str
\end{code}
