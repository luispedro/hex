{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module TestExpanded
    ( expandedTestGroup
    ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import qualified Data.Text.Lazy as LT
import Tokens
import CharStream (asqueue,TypedCharStream(..))
import Macros (expand)
import Defaults (plaintexenv)

expandedTestGroup = $(testGroupGenerator) 


simple_expand :: LT.Text -> String
simple_expand = concatMap show . expanded . asqueue "<input>" . convert
    where
        expanded = expand . newTokenStream . TypedCharStream plaintexenv
        convert = LT.map (\c -> if c == '|' then '\\' else c)

case_csea = simple_expand csea @?= "<b><c>(bye)"
    where
        csea = "|def|bc{bc}%\n\
                \|def|a{|b}%\n\
                \|csname|expandafter|string|a c|endcsname%\n\
                \|bye\n"

case_setbox = simple_expand setbox @?= "<setbox 0 = [<\\hbox>,<h>,<e>,<l>,<l>,<o>]><box[0]><\\par>"
    where
        setbox = "|setbox0=|hbox{hello}|box0|par"

