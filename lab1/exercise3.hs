module Exercise3 (test) where

import Data.List
import Test.QuickCheck

factorial :: Int -> Int
factorial n = product[1..n]
{- Alternative implementation. -}
{- factorial 0 = 1 -}
{- factorial n = n * (factorial (n-1)) -}

{- Number of distinctive permutations of a finite set of n elements = n! -}

permutationsComp :: Positive Int -> Bool
permutationsComp (Positive n) = factorial n == length (permutations [1..n])

test :: IO()
test = verboseCheck permutationsComp

{--TODO: Compare permutations function with perms from the slides, cannot test because order is
different --> could implement equals that ignores order (see simons solution (last 3 lines)) --}

{- Why would order matter if we are only interested in: -}
{- _1. result of factorial -}
{- _2. length of permutations -}

{- Testing whether the order of the standard library function and our functions are
- equal does not add anything to the solution if we already have the length, the length of the lements
- matters not the order they appear in. Unless ofcourse I am misunderstanding the assignment -}

{-

Answers to the questions:
=========================
//TODO: Explain why verbose Check --> see which numbers take "forever"

# When you perform the test for exercise 5, what are you testing actually?
> We're checking here a part of the secification, fir sure not a mathematical fact.

Maybe it's a mathematical fact since with every N the length of the permutations grows with
N?

-}
