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

test_token1 = (length $ chars2tokens $ map (annotate plaintextable) "\\macro") ~=? 1
test_token5 = (length $ chars2tokens $ map (annotate plaintextable) "macro") ~=? 5
\end{code}

The main driver

\begin{code}
tests = TestList [test_categoryCode_codeCategory, test_token1, test_token5]
main = runTestTT tests
\end{code}
