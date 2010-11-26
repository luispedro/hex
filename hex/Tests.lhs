Unit tests are their own programme.
\begin{code}
module Main where
\end{code}

Import basic functionality and our own modules

\begin{code}
import Test.HUnit
import Debug.Trace

import Chars
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

The main driver

\begin{code}
tests = test_categoryCode_codeCategory
main = runTestTT tests
\end{code}
