{-# LANGUAGE TemplateHaskell #-}
-- Unit tests are their own programme.

module Main where

-- Import basic functionality and our own modules

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Chars
import CharStream
import Tokens
import String
import Macros
import Linebreak
import Modes (vMode, paragraph)
import Defaults (startenv, plaintexenv)

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
