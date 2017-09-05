import Data.List
import Test.QuickCheck

fractional :: Int -> Int
fractional (n) = product[1..n]

{- Number of distinctive permutations of a finite set of n elements = n! -}

permutationsComp :: Positive Int -> Bool
permutationsComp (Positive n) = fractional n == length (permutations [1..n])

test = quickCheckResult permutationsComp

{-

Answers to the questions:
=========================

# When you perform the test for exercise 5, what are you testing actually?
> We're checking here a part of the secification, fir sure not a mathematical fact.

-}
