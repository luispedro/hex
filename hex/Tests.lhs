Unit tests are their own programme.
\begin{code}
module Main where
\end{code}

Import basic functionality and our own modules

\begin{code}
import Test.HUnit
import Debug.Trace

import Chars
import Tokens
import String
import Macros
import Linebreak
\end{code}

Little helper function

\begin{code}
tracex x = trace (show x) x
\end{code}

Test whether the categoryCode and codeCategory functions are consistent:

\begin{code}
test_categoryCode_codeCategory = TestList 
        [ (i ~=? (cac i)) | i <- [0..15]]
    where cac = codeCategory . categoryCode
\end{code}

Test simple parsing:

\begin{code}
test_token1 = (length $ chars2tokens $ map (annotate plaintextable) "\\macro") ~=? 1
test_token5 = (length $ chars2tokens $ map (annotate plaintextable) "macro") ~=? 5
test_sS = (length $ sS $ map (annotate plaintextable) "     ") ~=? 0
test_sM1 = (length $ sM $ map (annotate plaintextable) "     ") ~=? 1
test_sM2 = (length $ sM $ map (annotate plaintextable) "a    ") ~=? 2
\end{code}

Simple string tests for find.

\begin{code}
test_stringfind = (find "abc" "012abc") ~=? Just 3
test_stringnotfind = (find "abc" "012abd") ~=? Nothing
\end{code}

Tests for line breaking

\begin{code}
test_paragraphs = (length $ paragraphs [PrimitiveCommand  "par"]) ~=? 1
\end{code}

The main driver

\begin{code}
tests = TestList [test_categoryCode_codeCategory
                    ,test_token1
                    ,test_token5
                    ,test_sS
                    ,test_sM1
                    ,test_sM2
                    ,test_stringfind
                    ,test_stringnotfind
                    ,test_paragraphs
                    ]
main = runTestTT tests
\end{code}
