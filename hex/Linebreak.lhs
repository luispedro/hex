\section{Line Breaking}
\begin{code}
module Linebreak (
    breakintolines,
    concatenatewords,
    ) where

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

        nat_exp_shr = V.scanl props (zeroDimen,zeroDimen,zeroDimen) velems
        props (w,st,sh) (B.EGlue g) = (w `dplus` (B.size g), st `dplus` (B.expandable g), sh `dplus` (B.shrinkage g))
        props (w,st,sh) (B.EBox b) = (w `dplus` (B.width b), st, sh)
        props s _ = s

        bestfit :: Int -> Int -> (Ratio Integer,[Int])
        bestfit s e = (bfcache ! s) ! e
            where bfcache = V.generate (n+1) (\s -> V.generate (n+1) (bestfit' s))
        bestfit' s e
            | (s >= e) = error "hex.texBreak.bestfit': Trying to fit an empty array!"
            | (e == s+1) = (demerits s e, [s,e])
            | otherwise =
                    if bestbreak == e
                        then (demerits s e,[s,e])
                        else (bestval,(s:bestbreak:(tail $ snd $ bestfit bestbreak e)))
            where
                (bestbreak,bestval) = trybreaks (e,demerits s e) [(s+1)..(e-1)]
                minsum :: Ratio Integer -> Ratio Integer -> Ratio Integer -> Ratio Integer
                minsum lim a b = if a >= lim then lim else min lim (a+b)
                trybreaks :: (Int,Ratio Integer) -> [Int] -> (Int, Ratio Integer)
                trybreaks r [] = r
                trybreaks (b,v) (m:ms) =
                        if v <= vm
                            then trybreaks (b,v) ms
                            else trybreaks (m,vm) ms
                    where
                        vm = minsum v (demerits s m) (tdemerits m e)
        demerits s e = assert (e > s) $ (cache ! s) ! e
            where
                cache = V.generate (n+1) (\i -> V.generate (n+1) (demerits' i))
                demerits' s e
                    | (e == s+1) = singledemerit $ velems ! s
                    | otherwise = dfor e s
                    where
                        singledemerit (B.EPenalty _) = plus_inf
                        singledemerit (B.EGlue _) = plus_inf
                        singledemerit (B.EBox b) = dfor e s
        dfor e s = if r < -1 then plus_inf else 100*(abs r)*(abs r)*(abs r)
            where
                r = delta `sdratio` (if delta `dgt` zeroDimen then tshrinkage else texpandable)
                delta = naturalsize `dsub` textwidth
                naturalsize = nt_e `dsub` nt_s
                tshrinkage = sh_e `dsub` sh_s
                texpandable = ex_e `dsub` ex_s
                (nt_s,ex_s,sh_s) = nat_exp_shr ! s
                (nt_e,ex_e,sh_e) = nat_exp_shr ! e
                num `sdratio` denom =
                    if denom `dgt` zeroDimen
                        then num `dratio` denom
                        else if num `dgt` zeroDimen then plus_inf else neg_inf
        tdemerits s e = (tcache ! s) ! e
            where
                tcache = V.generate (n+1) (\s -> V.generate (n+1) (tdemerits' s))
                tdemerits' s e = sumds 0 $ snd $ bestfit s e
                sumds v [] = v
                sumds v [_] = v
                sumds v (b0:b1:bs) = sumds (v + (demerits b0 b1)) (b1:bs)
        plus_inf = (1000000000000000000 :: Ratio Integer)
        neg_inf = (-1000000000000000000 :: Ratio Integer)
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

