\section{Line Breaking}
\begin{code}
module Linebreak (
    breakintolines,
    concatenatewords,
    demerit,
    _texBreak, -- These are internal functions, exported for testing only
    _preprocessParagraph,
    _acc_sizes,
    ) where

import Data.Maybe
import Data.Ratio
import qualified Data.Vector as V
import Data.Vector ((!),(!?))
import Data.Vector.Algorithms.Intro (sortBy)
import Control.Monad.ST

import qualified Boxes as B
import Measures
\end{code}

Line breaking is one of \TeX{}'s traditional strengths.

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
--leFlag (B.EBox _) = False
--leFlag (B.EGlue _) = False
--leFlag (B.EPenalty p) = B.flag p
\end{code}

A helper function to fix a glue to a particular size.

\begin{code}
fixGlue (B.Glue s _ _ _)= B.Box
                { B.boxType=B.H
                , B.width=s
                , B.depth=zeroDimen
                , B.height=zeroDimen
                , B.boxContents=(B.Kern s)
                }
\end{code}

We now define some basic constants: infinite and minus-infinite penalties.
\begin{code}
penalty :: Integer -> B.HElement
penalty p = B.EPenalty $ B.Penalty B.H p False
infPenalty = 10000
minfPenalty = (-10000)
\end{code}

In order to handle divisions by zero, we introduce infinities and the safe
ratio function (infinities are also used elsewhere):

\begin{code}
num `sdratio` denom
    | denom /= zeroDimen =  num `dratio` denom
    | num `dgt` zeroDimen = plus_inf
    | otherwise = neg_inf
plus_inf = (1000000000000000000 :: Ratio Integer)
neg_inf = (-1000000000000000000 :: Ratio Integer)
\end{code}

In order to make everything work out, we need to add special markers to the end
of the paragraph. Currently, they are not used, but, when the full algorithm is
implemented, they will guarantee that the last line of a paragraph is correctly
broken and filled out.

\begin{code}
_preprocessParagraph pars = pars ++
                                [ penalty infPenalty
                                , spaceEGlue
                                , penalty minfPenalty]
    where spaceEGlue = B.EGlue $ B.Glue
            { B.size=zeroDimen
            , B.expandable=(dimenFromInches 1000)
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
        (firstelems,rest) = span isBox cs
        first = merge $ map getBox firstelems
        getBox (B.EBox b) = b
        getBox _ = error "hex.concatenatewords.getBox: Not a box!"
        merge bs = B.EBox $ B.mergeBoxes B.H bs
        isBox (B.EBox _) = True
        isBox _ = False
\end{code}

Now we come to the main function: \code{breakParagraphIntoLines} and a small
toggle switch (currently hardcoded) to choose between the \emph{first fit} and
the \emph{tex fit} algorithms.

\begin{code}
useTexFit = True
\end{code}

\begin{code}
breakParagraphIntoLines w elems = breakat w elems' $ algo w elems'
    where
        algo = if useTexFit then _texBreak else firstFit
        elems' = _preprocessParagraph elems
\end{code}

\code{_texBreak} implements the \TeX{} line breaking algorithm.

\code{breakat} takes the elements and a list of breaks and returns a list of
Vboxes.

\begin{code}
breakat :: Dimen -> [B.HElement] -> [Int] -> [B.VBox]
breakat w elems breaks = breakat' elems $ diffs breaks
    where
        diffs (b0:b1:bs) = ((b1-b0):diffs (b1:bs))
        diffs [_] = []
        diffs _ = []
        breakat' _ [] = []
        breakat' es (b:bs) = (packagebox w $ take b es):(breakat' (drop b es) bs)
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

The basic goal of line-breaking is to minimise demerits:
\begin{code}
demerit textwidth velems nat_exp_shr s ell = if canbreak then badness + curpenalty else plus_inf
    where
        n = V.length velems
        canbreak = if e > n then False else case (velems !? (e-2), velems !? (e-1)) of
            (Just _,Just (B.EPenalty _)) -> True
            (Just (B.EBox _), Just (B.EGlue _)) -> True
            _ -> False
        e = s + ell + 1
        ee = case velems ! (e-1) of
            B.EGlue _ -> e-1
            _ -> e
        curpenalty = fromInteger $ lePenalty $ velems ! (e-1)
        badness = if r < -1 then plus_inf else 100*(abs r)*(abs r)*(abs r)
        r = delta `sdratio` (if delta `dgt` zeroDimen then texpandable else tshrinkage)
        delta = textwidth `dsub` naturalsize
        naturalsize = nt_e `dsub` nt_s
        tshrinkage = sh_e `dsub` sh_s
        texpandable = ex_e `dsub` ex_s
        (nt_s,ex_s,sh_s) = nat_exp_shr ! s
        (nt_e,ex_e,sh_e) = nat_exp_shr ! ee
\end{code}

This is equivalent to cumsum on the 3 dimensions. It makes it fast to compute
partial sums (i.e., sum (slice velems i j))
\begin{code}
_acc_sizes = V.scanl props (zeroDimen,zeroDimen,zeroDimen)
    where
        props (w,st,sh) e = (w `dplus` (leWidth e), st `dplus` (leStretch e), sh `dplus` (leShrink e))
\end{code}

This implements a slightly different algorithm than \TeX\ does. It is a simpler
and less efficient implementation as it does the full dynamic programming
without early exits that are easier to achieve in a more imperative style.
\begin{code}
_texBreak :: Dimen -> [B.HElement] -> [Int]
_texBreak _ [] = []
_texBreak textwidth elems = snd $ bfcache ! 0
    where
        velems = V.fromList elems
        n = V.length velems
        nat_exp_shr = _acc_sizes velems

        bestfit :: Int -> (Ratio Integer,[Int])
        bestfit s
            | (s > n) = error "hex._texBreak.bestfit': Trying to bestfit past the end!"
            | (s == n) = (0,[n])
            | otherwise = case V.toList $ vargsort demerits_s of
                    [] -> error "hex._texBreak.trybreaks: empty!"
                    (m:ms) -> let (val_m,fit_m) = bfcache ! (s+m+1) in
                                    trybreaks ((demerits_s ! m) + val_m, s:fit_m) ms
            where
                demerits_s = dtable ! s
                trybreaks :: (Ratio Integer, [Int]) -> [Int] -> (Ratio Integer, [Int])
                trybreaks r [] = r
                trybreaks cur@(v,_) (m:ms)
                    | first == plus_inf = cur
                    | v <= vm = trybreaks cur ms
                    | otherwise =  trybreaks (vm, s:breaks) ms
                    where
                        (valm, breaks) = bfcache ! (s+m+1)
                        vm = min v (first + valm)
                        first = (demerits_s ! m)

        bfcache = V.generate (n+1) bestfit
        dtable = V.generate (n+1) (\i -> V.generate (n-i) (demerit textwidth velems nat_exp_shr i))
\end{code}

\begin{code}
packagebox :: Dimen -> [B.HElement] -> B.VBox
packagebox width boxes = B.mergeBoxes B.V $ toBoxes $ B.hboxto width $ cleanEnds boxes
    where
        toBoxes = mapMaybe toBox
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

And here is the first fit algorithm:

\begin{code}
firstFit :: Dimen -> [B.HElement] -> [Int]
firstFit _ [] = []
firstFit textwidth lelems = (0:restbreaks)
    where
        restbreaks = map (+first) $ firstFit textwidth $ drop first lelems
        first = firstLine zeroDimen lelems
        firstLine _ [] = 0
        firstLine (Dimen 0) (le:les) = 1 + (firstLine (leWidth le) les)
        firstLine n (le:les)
            | n' > textwidth = 0
            | otherwise = 1 + (firstLine n' les)
            where n' = (n `dplus` (leWidth le))
\end{code}

The interface function is \code{breakintolines}, which converts a list of
horizontal elements into a list of lines.

\begin{code}
breakintolines :: Dimen -> [B.HElement] -> [B.VBox]
breakintolines lw ls = breakParagraphIntoLines lw $ concatenatewords ls
\end{code}

