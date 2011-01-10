\begin{code}
module LoadPL where
import List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Char

import String

\end{code}

To store the information about a gliph, we use a floating point representation.
It isn't the best way since most of the fonts are in fixed point format, but it
will work for now.

\begin{code}
newtype FixWord = FixWord Float
fixToFloat (FixWord f) = f

instance Show FixWord where show (FixWord w) = show w
data GliphMetric = GliphMetric 
                        { character :: Char
                        , width :: FixWord
                        , height :: FixWord
                        , depth :: FixWord
                        , italicCorrection :: FixWord
                        }
instance Show GliphMetric where
    show g = "G[" ++ [character g]
                ++ ", wd: " ++ (show $ width g)
                ++ ", ht: " ++ (show $ height g)
                ++ ", dp: " ++ (show $ depth g)
                ++ ", ic: " ++ (show $ italicCorrection g)
                ++ "]"

data FontInfo = FontInfo [GliphMetric]
instance Show FontInfo where
    show (FontInfo fi) = concat $ map ((++"\n") . show) fi

widthHeightDepth (FontInfo fi) c = widthHeightDepth' fi c
    where
        widthHeightDepth' [] c = error ("Not found character:" ++ [c] ++ "(" ++ (show $ ord c) ++ ")")
        widthHeightDepth' ((GliphMetric ch w h d _):gms) c
            | c == ch = (w,h,d)
            | otherwise = widthHeightDepth' gms c
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
        w = getprop "CHARWD R "
        h = getprop "CHARHT R "
        d = getprop "CHARDP R "
        it = getprop "CHARIC R "
        getprop start = case find start input of
                    Nothing -> FixWord 0.0
                    Just n -> FixWord $ (*base) $ read $ takeWhile (/=')') $ drop (n+ (length start)) input
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

\begin{code}
retrieveDesignSize (s:ss) = if isPrefixOf "DESIGNSIZE" s then retrieveDesignSize' s else retrieveDesignSize ss
    where
        retrieveDesignSize' :: String -> Float
        retrieveDesignSize' = read . drop (length "DESIGNSIZE R ")
\end{code}

Now just need to call the \code{readGliphInformation} function for each
S-expression.

\begin{code}
loadPL str = FontInfo $ catMaybes $ (map (readGliphInformation designsize)) $ breakIntoSExpressions str
    where
        designsize = retrieveDesignSize $ breakIntoSExpressions str
\end{code}
