{-# LANGUAGE TemplateHaskell #-}
-- Unit tests are their own programme.

module Main where

-- Import basic functionality and our own modules

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import System.IO.Unsafe
import qualified Data.Vector as V
import Text.Parsec hiding (many, optional, (<|>))



import Chars
import CharStream
import Hex (readFont)
import Tokens
import String
import Macros
import Modes (_paragraph, ModeState(..), _vModeM)
import Defaults (startenv,plaintexenv)
import Measures
import Linebreak
import ParseTFM
import FixWords
import qualified Environment as E
import qualified Fonts as F
import qualified Boxes as B

-- The main driver

main = $(defaultMainGenerator)

-- Test whether the categoryCode and codeCategory functions are consistent:

case_categoryCode_codeCategory = [0..15] @=? (map (codeCategory .categoryCode) [0..15])

-- Test simple parsing:

plaintextable = plaintexenv
case_token1 = (length $ chars2tokens "\\macro") @=? 1
case_token_par = (length $ chars2tokens "\\par") @=? 1
case_token_par_nl = (length $ chars2tokens "\\par\n") @=? 2
case_token_a_nl = (length $ chars2tokens "a\n") @=? 2
case_token_sp_nl = (length $ chars2tokens " \n") @=? 1
case_token_a_sp_nl = (length $ chars2tokens "a \n") @=? 2
case_token5 = (length $ chars2tokens "macro") @=? 5
--case_sS = (length $ applyStateFunction sS $ map (annotate plaintextable) "     ") @=? 0
--case_sM1 = (length $ applyStateFunction sM $ map (annotate plaintextable) "     ") @=? 1
--case_sM2 = (length $ applyStateFunction sM $ map (annotate plaintextable) "a    ") @=? 2

case_readNumberM5 = n @=? 5
    where
        (n,_) = runTkS readNumberM (asTokenStream "5\\relax")

case_readNumberM15 = n @=? 15
    where
        (n,_) = runTkS readNumberM (asTokenStream "15\\relax")

case_readNumberMfollow = cs @=? "\\relax"
    where
        (ControlSequence cs,_) = runTkS (readNumberM >> gettokenM) (asTokenStream "15\\relax")

case_readNumberMfollowEmpty = (emptyTokenStream rest) @=? True
    where
        (ControlSequence _,rest) = runTkS (readNumberM >> gettokenM) (asTokenStream "15\\relax")

asTokenStream s = (newTokenStream $ TypedCharStream plaintexenv s)

-- Simple string tests for find.

case_stringfind = (find "abc" "012abc") @=? Just 3
case_stringnotfind = (find "abc" "012abd") @=? Nothing

-- Tests for line breaking
case_paragraphs = (length p) @?= 0
    where Right p = runP _paragraph (ModeState undefined) "<test>" [PrimitiveCommand  "\\par"]

-- Tests for Modes
case_modes_empty = (length m) @?= 0
    where Right m = runP _vModeM (ModeState undefined) "<test>" []

case_modes_2 = (length m) @?= 2
    where
        Right m = runP _vModeM (ModeState undefined) "<test>" [ofc,ofc]
        ofc = OutputfontCommand undefined

case_modes_32 = (length m) @?= 32
    where
        Right m = runP _vModeM (ModeState undefined) "<test>" $ take 32 $ repeat ofc
        ofc = OutputfontCommand undefined

case_mode_complex = (length m) @?= 5
    where
        Right m = runP _vModeM (ModeState e) "<test>" [ofc,sfc,PrimitiveCommand "\\par",sfc,PrimitiveCommand "\\par"]
        ofc = OutputfontCommand (undefined, undefined)
        sfc = SelectfontCommand 0 (undefined, undefined)
        e = (E.setfont 0 undefined startenv)

-- Test for macro parsing
case_bracebrace = (chars2tokens "{a}") @=? parsed
    where parsed = fst $ _breakAtGroupEnd (tokenliststream $ chars2tokens "{a}}")

-- Test B.hboxto
di = dimenFromInches
textwidth = di 5
totalwidth = (foldr1 dplus) . (map B.esize)
hbox3elems = B.hboxto textwidth elems
    where
        elems = [
                B.EBox (B.Box B.H (di 0) (di 1) (di 2) (B.CharContent 'S' 0)),
                B.EGlue (B.Glue B.H (di 1) (di 2) (di 2) 0),
                B.EBox (B.Box B.H (di 0) (di 1) (di 1) (B.CharContent 'E' 0))
                ]
hbox3elems_sh = B.hboxto textwidth elems
    where
        elems = [
                B.EBox (B.Box B.H (di 2) (di 2) (di 2) (B.CharContent 'S' 0)),
                B.EGlue (B.Glue B.H (di 2) (di 2) (di 2) 0),
                B.EBox (B.Box B.H (di 2) (di 2) (di 2) (B.CharContent 'E' 0))
                ]
case_hbox_width = (totalwidth hbox3elems) @?= textwidth
case_hbox_width_sh = (totalwidth hbox3elems_sh) @?= textwidth

case_hbox_nr_elems = (length hbox3elems) @?= 3
case_hbox_glue =
    case (hbox3elems !! 1) of
        B.EGlue g -> (B.size g) @?= (dimenFromInches 2)
        _ -> error "should have matched!"

case_font_dq = ((fixToFloat w) > 4.9) @?= True
    where
        (w,_,_) = F.widthHeightDepth cmr10font '"'

case_font_space = ((fixToFloat w) > 3.0) @?= True
    where
        F.SpaceInfo w _ _ = F.spaceInfo cmr10font


cmr10font = unsafePerformIO $ do
    (_,fi) <- readFont "cmr10"
    return fi

case_demerits = [0,0,0,0] @=?
            [ (demerit w velems nat_exp_shr 0 3)
            , (demerit w velems nat_exp_shr 2 3)
            , (demerit w velems nat_exp_shr 4 3)
            , (demerit w velems nat_exp_shr 10 3)
            ]
    where
        -- The exact numbers are meaningless, but I want to have exact values
        -- Same thing below
        w = Dimen 50
        velems = V.fromList elems
        nat_exp_shr = _acc_sizes velems
        elems = rep (16 :: Int)

rep :: Int -> [B.HElement]
rep 0 = []
rep n = (x:sp:rep (n-1))

sp = B.EGlue (B.Glue B.H (Dimen 10) (Dimen 5) (Dimen 4) 0)
x = B.EBox (B.Box B.H zeroDimen zeroDimen (Dimen 20) xc)
xc = B.CharContent 'x' 0

case_demerits_squeeze = assert $ allsame
            [ (demerit w velems nat_exp_shr 0 3)
            , (demerit w velems nat_exp_shr 2 3)
            , (demerit w velems nat_exp_shr 4 3)
            , (demerit w velems nat_exp_shr 10 3)
            ]
    where
        w = Dimen 47
        velems = V.fromList elems
        nat_exp_shr = _acc_sizes velems
        elems = rep (16 :: Int)
        allsame [a,b] = (a == b)
        allsame (a:b:bs) = (a == b) && allsame (b:bs)
        allsame _ = error "allsame"

case_texBreak = [0,4,8,12,16,20,24,28,32] @=? _texBreak w elems
    where
        w = Dimen 50
        elems = rep 16

case_texBreak_small = [0,6] @=? (_texBreak w $ _preprocessParagraph elems)
    where
        -- The exact numbers are meaningless, but I want to have exact values
        w = Dimen 50
        elems = [x,sp,x]

case_acc = [0,20,30,50,50,50,50] @=? (map (\(Dimen s,_,_) -> s) $ V.toList $ _acc_sizes $ V.fromList $ _preprocessParagraph elems)
    where elems = [x,sp,x]

case_acc_length = (n+1) @=? (V.length $ _acc_sizes velems)
    where
        elems = [x,sp,x]
        velems = V.fromList $ _preprocessParagraph elems
        n = V.length velems

case_value_penalty = (-10000) @=? (demerit w velems nat_exp_shr 0 (n-1))
    where
        pre_elems = [x,sp,x]
        elems = _preprocessParagraph pre_elems
        n = length elems
        w = Dimen 50
        velems = V.fromList elems
        nat_exp_shr = _acc_sizes velems


case_break_byte = (10,15) @=? (_breakByte 4 (0xaf::Int))

case_fixword_1 = (f0*f1) @?= ((fromInteger 1) :: FixWord)
    where
        f0 = (fromInteger 1) :: FixWord
        f1 = (fromInteger 1) :: FixWord

case_fixword_1float = (fixToFloat $ f0*f1) @?= 1.0
    where
        f0 = (fromInteger 1) :: FixWord
        f1 = (fromInteger 1) :: FixWord

case_fixword_1_2 = (f0+f1) @?= ((fromInteger 2) :: FixWord)
    where
        f0 = (fromInteger 1) :: FixWord
        f1 = (fromInteger 1) :: FixWord

-- Beh is just for the benefit of the Arbitrary instance
-- Otherwise, we would have an orphan instance declaration.
data Beh = Beh { beh :: B.HElement } deriving (Show)
instance Arbitrary Beh where
    arbitrary = frequency [(5,elements [Beh x]), (1,elements [Beh sp])]

prop_list belems = valid (_texBreak w $ _preprocessParagraph elems)
    where
        _types = belems :: [Beh]
        elems :: [B.HElement]
        elems = map beh belems
        w = Dimen 50
        valid [] = False
        valid [_] = False
        valid xs | head xs /= 0 = False
        valid xs = monotonic xs
        monotonic [] = True
        monotonic [_] = True
        monotonic (a:b:xs) = (a < b) && monotonic (b:xs)

