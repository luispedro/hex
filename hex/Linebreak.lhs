\section{Line Breaking}
\begin{code}
module Linebreak where

import Data.Maybe
import Data.Ratio
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.List.Extras.Argmax
import Control.Exception

import Macros
import qualified Boxes as B
import Measures
import Tokens
import Chars

\end{code}

Line breaking is one of TeX's traditional strengths.

For line breaking, we need to first transform all of the elements into line
elements. We are following the chapter ``Breaking Paragraphs Into Lines'' in
Digital Typography by D.~E. Knuth.

\begin{code}

leWidth (B.EBox hb) = B.width hb
leWidth (B.EGlue g) = B.size g
leWidth (B.EPenalty p) = zeroDimen
leStretch (B.EBox hb) = zeroDimen
leStretch (B.EGlue g) = B.expandable g
leStretch (B.EPenalty p) = zeroDimen
leShrink (B.EBox hb) = zeroDimen
leShrink (B.EGlue g) = B.shrinkage g
leShrink (B.EPenalty p) = zeroDimen
lePenalty (B.EBox _) = 0
lePenalty (B.EGlue _) = 0
lePenalty (B.EPenalty p) = B.value p
leFlag (B.EBox _) = False
leFlag (B.EGlue _) = False
leFlag (B.EPenalty p) = B.flag p
\end{code}

A helper function to fix a glue to a particular size.

\begin{code}
fixGlue (B.Glue _ s _ _ _)= B.Box
                { B.boxType=B.H
                , B.width=s
                , B.depth=zeroDimen
                , B.height=zeroDimen
                , B.boxContents=(B.Kern s)
                }
\end{code}

We now define some basic constants: infinite and minus-infinite penalties.
\begin{code}
penalty p = B.EPenalty $ B.Penalty B.H p False
infPenalty = 10000
minfPenalty = (-10000)
\end{code}

In order to make everything work out, we need to add special markers to the end
of the paragraph. Currently, they are not used, but, when the full algorithm is
implemented, they will guarantee that the last line of a paragraph is correctly
broken and filled out.

\begin{code}
preprocessParagraph pars = pars ++
                                [ penalty infPenalty
                                , spaceEGlue
                                , penalty minfPenalty]
    where spaceEGlue = B.EGlue $ B.Glue
            { B.glueType=B.H
            , B.size=zeroDimen
            , B.expandable=zeroDimen
            , B.shrinkage=zeroDimen
            , B.infLevel=1
            }
\end{code}

In order to break up lines \emph{only at word boundaries}, we merge words
(sequences of \code{HBox}es separated by \code{Glue} elements) into single
boxes (words). This is independent of what the words actually consist of (e.g.,
they may contain symbols or other non-alphabetic elements).

Hyphenation is, currently, not supported.

\begin{code}
concatenatewords [] = []
concatenatewords (le@(B.EGlue g):les) = (le:concatenatewords les)
concatenatewords cs = (first:concatenatewords rest)
    where
        (firstelems,rest) = break (not . isBox) cs
        first = merge $ map getBox firstelems
        getBox (B.EBox b) = b
        merge bs = B.EBox $ B.mergeBoxes B.H bs
        isBox (B.EBox _) = True
        isBox _ = False
\end{code}

Now we come to the main function: \code{breakParagraphIntoLines}. It currently
uses the \emph{first fit} algorithm.

\begin{code}
breakParagraphIntoLines = texBreak
\end{code}

\code{texBreak} implements the \TeX{} line breaking algorithm.

\begin{code}
breakat _ _ _ [] = []
breakat w n elems (b:bs) = (packagebox w $ take (b-n) elems):(breakat w b (drop (b-n) elems) bs)
\end{code}

\begin{code}
texBreak :: Dimen -> [B.HElement] -> [B.VBox]
texBreak _ [] = []
texBreak textwidth elems = breakat textwidth 0 elems $ snd $ bestfit 0 n
    where
        velems = V.fromList elems
        n = V.length velems
        bestfit :: Int -> Int -> (Ratio Integer,[Int])
        bestfit s e = (bfcache ! s) ! e
            where bfcache = V.generate (n+1) (\s -> V.generate (n+1) (bestfit' s))
        bestfit' s e
            | (s >= e) = error "hex.texBreak.bestfit': Trying to fit an empty array!"
            | (e == s+1) = (demerits s e, [s,e])
            | otherwise = if bestval < demerits s e then (bestval, recursivebreak)  else (demerits s e, [s,e])
            where
                (bestbreak,bestval) = trybreaks plus_inf [(s+1)..(e-1)]
                minsum lim a b = if a > lim then lim else min lim (a+b)
                trybreaks v [m] = (m, minsum v (tdemerits s m) (tdemerits m e))
                trybreaks v (m:ms) = if vm < rm then (m, vm) else (r, rm)
                        where
                            vm = minsum v (tdemerits s m) (tdemerits m e)
                            (r,rm) = trybreaks vm ms
                (_, firstfit) = bestfit s bestbreak
                (_, (_:secondfit)) = bestfit bestbreak e
                recursivebreak = firstfit ++ secondfit
        demerits s e = assert (e > s) $ (cache ! s) ! e
            where
                cache = V.generate (n+1) (\i -> V.generate (n+1) (demerits' i))
                demerits' s e
                    | (e == s+1) = singledemerit $ velems ! s
                    | otherwise = if r < -1 then plus_inf else 100*(abs r)*(abs r)*(abs r)
                    where
                        r = fit $ V.slice s (e-s) velems
                        singledemerit (B.EPenalty _) = plus_inf
                        singledemerit (B.EGlue _) = plus_inf
                        singledemerit (B.EBox b) = 100*r*r*r
                            where r = let wb = B.width b in if wb `dgt` textwidth then wb `dratio` textwidth else textwidth `dratio` wb
        fit es = sum $ map ((^3) . stretchof) $ zip nbox les
            where
                les = V.toList es
                nbox = B.hboxto textwidth les
                stretchof ((B.EGlue p),(B.EGlue a)) = delta `dratio` param
                    where
                        delta = (B.size a) `dsub` (B.size p)
                        param = if (B.size a) `dgt` (B.size p) then (B.expandable p) else (B.shrinkage p)
                stretchof _ = 0
        tdemerits s e = (tcache ! s) ! e
            where
                tcache = V.generate (n+1) (\s -> V.generate (n+1) (tdemerits' s))
                tdemerits' s e = sum $ map (\(a,b) -> demerits a b) (pairs $ snd $ bestfit s e)
        pairs [] = []
        pairs [e] = []
        pairs (e0:e1:es) = ((e0,e1):pairs (e1:es))
        plus_inf = 100000000
\end{code}

\begin{code}
packagebox :: Dimen -> [B.HElement] -> B.VBox
packagebox width boxes = B.mergeBoxes B.V $ toBoxes $ B.hboxto width boxes
    where
        toBoxes = catMaybes . (map toBox)
        toBox (B.EBox b) = Just b
        toBox (B.EGlue g) = Just $ fixGlue g
        toBox _ = Nothing
\end{code}

And here is the first fit algorithm:

\begin{code}
firstFit :: Dimen -> [B.HElement] -> [B.VBox]
firstFit _ [] = []
firstFit lineWidth les = (B.mergeBoxes B.V $ toBoxes $ B.hboxto lineWidth $ cleanEnds first):(firstFit lineWidth rest)
    where
        (first,rest) = splitAt (firstLine zeroDimen les) les
        firstLine _ [] = 0
        firstLine (Dimen 0) (le:les) = 1 + (firstLine (leWidth le) les)
        firstLine n (le:les)
            | n' > lineWidth = 0
            | otherwise = 1 + (firstLine n' les)
            where n' = (n `dplus` (leWidth le))
        toBoxes = catMaybes . (map toBox)
        toBox (B.EBox b) = Just b
        toBox (B.EGlue g) = Just $ fixGlue g
        toBox _ = Nothing
        cleanEnds = removeFirst . removeLast
        removeFirst [] = []
        removeFirst (e:es) = case e of
            B.EGlue _ -> removeFirst es
            _ -> (e:es)
        removeLast [] = []
        removeLast (e:es) = case e of
            B.EGlue _ -> if nomore es then [] else (e:removeLast es)
            _ -> (e:removeLast es)
        nomore [] = True
        nomore (B.EGlue _:es) = nomore es
        nomore _ = False
\end{code}

The interface function is \code{breakintolines}, which converts a list of
horizontal elements into a list of lines.

\begin{code}
breakintolines :: Dimen -> [B.HElement] -> [B.VBox]
breakintolines lw ls = breakParagraphIntoLines lw $ concatenatewords ls
\end{code}

