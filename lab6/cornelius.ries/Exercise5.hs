module Exercise5 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6
import Exercise3
import Exercise4

-- Time spent:

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
       k <- [2..],
       prime (6*k+1),
       prime (12*k+1),
       prime (18*k+1) ]

-- smallestF 1 carmichael
-- false positive: 294409
--
-- there is only one false positive - either the rest takes too long or there are none

{-

  The wikipedia article states that the carmichael  prime numbers are all
  fermat valid pseudo prime numbers
  
  -> all carmichael numbers should give a false positive

-}
