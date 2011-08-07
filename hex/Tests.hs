{-# LANGUAGE TemplateHaskell #-}
-- Unit tests are their own programme.

module Main where

-- Import basic functionality and our own modules

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Chars
import Tokens
import String
import Macros
import Modes (paragraph)
import Defaults (plaintexenv)
import Measures
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

-- Simple string tests for find.

case_stringfind = (find "abc" "012abc") @=? Just 3
case_stringnotfind = (find "abc" "012abd") @=? Nothing

-- Tests for line breaking
case_paragraphs = (length p, length r) @=? (0,0)
        where (p,r) = (paragraph undefined [PrimitiveCommand  "\\par"])

-- Test B.hboxto
di = dimenFromInches
textwidth = di 5
totalwidth = (foldr1 dplus) . (map B.esize)
hbox3elems = B.hboxto textwidth elems
    where
        elems = [
                B.EBox (B.Box B.H (di 0) (di 1) (di 2) (B.TextContent "S")),
                B.EGlue (B.Glue B.H (di 1) (di 2) (di 2) 0),
                B.EBox (B.Box B.H (di 0) (di 1) (di 1) (B.TextContent "E"))
                ]
hbox3elems_sh = B.hboxto textwidth elems
    where
        elems = [
                B.EBox (B.Box B.H (di 2) (di 2) (di 2) (B.TextContent "S")),
                B.EGlue (B.Glue B.H (di 2) (di 2) (di 2) 0),
                B.EBox (B.Box B.H (di 2) (di 2) (di 2) (B.TextContent "E"))
                ]
case_hbox_width = (totalwidth hbox3elems) @?= textwidth
case_hbox_width_sh = (totalwidth hbox3elems_sh) @?= textwidth

case_hbox_nr_elems = (length hbox3elems) @?= 3
case_hbox_glue =
    case (hbox3elems !! 1) of
        B.EGlue g -> (B.size g) @?= (dimenFromInches 2)
        _ -> error "should have matched!"

