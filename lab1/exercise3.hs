module Exercise3 (test) where

import Data.List
import Test.QuickCheck

fractional :: Int -> Int
fractional n = product[1..n]

{- Number of distinctive permutations of a finite set of n elements = n! -}

permutationsComp :: Positive Int -> Bool
permutationsComp (Positive n) = fractional n == length (permutations [1..n])

test :: IO()
test = verboseCheck permutationsComp

{--TODO: Compare permutations function with perms from the slides, cannot test because order is
different --> could implement equals that ignores order (see simons solution (last 3 lines)) --}

{-

Answers to the questions:
=========================
//TODO: Explain why verbose Check --> see which numbers take "forever"

# When you perform the test for exercise 5, what are you testing actually?
> We're checking here a part of the secification, fir sure not a mathematical fact.

-}
