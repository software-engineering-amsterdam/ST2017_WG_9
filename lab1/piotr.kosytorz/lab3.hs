import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

fractional :: Int -> Int
fractional = \ n -> product[1..n]

{- Number of distinctive permutations of a finite set of n elements = n! -}

permutationsComp :: Int -> Bool
permutationsComp = \ n -> n >= 0 --> fractional n == length (permutations [1..n])

test = quickCheckResult permutationsComp

{-

Answers to the questions:
=========================

# When you perform the test for exercise 5, what are you testing actually?
> We're checking here a part of the secification, fir sure not a mathematical fact.

-}
