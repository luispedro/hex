module Tracex (trace, tracex) where

import Debug.Trace

tracex x = trace (show x) x
