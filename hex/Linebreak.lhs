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
import Data.Vector.Algorithms.Intro (sortBy)
import Control.Monad.ST

import qualified Boxes as B
import Measures
\end{code}

Line breaking is one of TeX's traditional strengths.

For line breaking, we need to first transform all of the elements into line
elements. We are following the chapter ``Breaking Paragraphs Into Lines'' in
Digital Typography by D.~E. Knuth.

\begin{code}

leWidth (B.EBox hb) = B.width hb
leWidth (B.EGlue g) = B.size g
leWidth (B.EPenalty _) = zeroDimen
leStretch (B.EBox _) = zeroDimen
leStretch (B.EGlue g) = B.expandable g
leStretch (B.EPenalty _) = zeroDimen
leShrink (B.EBox _) = zeroDimen
leShrink (B.EGlue g) = B.shrinkage g
leShrink (B.EPenalty _) = zeroDimen
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
concatenatewords (le@(B.EGlue _):les) = (le:concatenatewords les)
concatenatewords cs = (first:concatenatewords rest)
    where
        (firstelems,rest) = break (not . isBox) cs
        first = merge $ map getBox firstelems
        getBox (B.EBox b) = b
        getBox _ = error "hex.concatenatewords.getBox: Not a box!"
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

We need to use a argsort function for vectors, which we define here:

\begin{code}
vargsort :: (Ord e) => V.Vector e -> V.Vector Int
vargsort vec = runST $ do
        indices <- V.unsafeThaw $ V.enumFromN 0 $ V.length vec
        sortBy comparison indices
        V.unsafeFreeze indices
    where comparison i j = if (vec ! i) == (vec ! j)
                    then compare i j
                    else compare (vec ! i) (vec ! j)
\end{code}

\code{minsum} is a small helper function which computes $\min{\ell, a + b}$,
but does not evaluate \code{b} if it is not necessary (we assume $a,b > 0$).

\begin{code}
minsum :: Ratio Integer -> Ratio Integer -> Ratio Integer -> Ratio Integer
minsum lim a b = if a >= lim then lim else min lim (a+b)
\end{code}

\begin{code}
texBreak :: Dimen -> [B.HElement] -> [B.VBox]
texBreak _ [] = []
texBreak textwidth elems = breakat textwidth 0 elems $ snd $ bestfit 0
    where
        velems = V.fromList elems
        n = V.length velems

        nat_exp_shr = V.scanl props (zeroDimen,zeroDimen,zeroDimen) velems
        props (w,st,sh) e = (w `dplus` (leWidth e), st `dplus` (leStretch e), sh `dplus` (leShrink e))

        bestfit :: Int -> (Ratio Integer,[Int])
        bestfit s = bfcache ! s
        bfcache = V.generate (n+1) bestfit'
        bestfit' s
            | (s >= n) = (0,[]) -- error "hex.texBreak.bestfit': Trying to bestfit past the end!"
            | otherwise = if bestbreak == (n-s-1)
                        then (demerits_s ! (n-s-1), [s,n])
                        else (bestval,(s:s+1+bestbreak:(tail $ snd $ bestfit $ s+1+bestbreak)))
            where
                (bestbreak,bestval) = trybreaks (0, demerits_s ! 0) $ V.toList $ vargsort demerits_s
                demerits_s = dtable ! s
                trybreaks :: (Int,Ratio Integer) -> [Int] -> (Int, Ratio Integer)
                trybreaks r [] = r
                trybreaks (b,v) (m:ms) = if v <= vm
                        then trybreaks (b,v) ms
                        else trybreaks (m,vm) ms
                    where
                        vm = minsum v (demerits_s ! m) (fst $ bestfit (s+m+1))
        dtable = V.generate (n+1) (\i -> V.generate (n-i) (demerit i))
        demerit s ell
            | ell == 0 = singledemerit $ velems ! s
            | otherwise = dfor s (s+1+ell)
            where
                singledemerit (B.EPenalty _) = plus_inf
                singledemerit (B.EGlue _) = plus_inf
                singledemerit (B.EBox _) = dfor s (s+1)
        dfor s e = if r < -1 then plus_inf else 100*(abs r)*(abs r)*(abs r)
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

