module Exercise2 (test) where

import Data.List
import Test.QuickCheck

{- We've agreed to use lists 1..n, which indicates that we work only with natural
numbers only -}
powerComp :: Positive Int -> Bool
powerComp (Positive n) = 2 ^ n == length (subsequences [1..n])

{-- Using verboseCheck to find out how much time passes between each step. --}
test :: IO ()
test = verboseCheck powerComp

{-

Answers to the questions:
=========================

# Is the property hard to test? If you find that it is, can you given a reason why?
It's maybe easy to write the test case, but its hard for a machine to execute the
test for large instances of n. Building subsequences for different has an
exponencial memory complexity ( O(2^n) )

# Give your thoughts on the following issue:
# when you perform the test for exercise 4, what are you testing actually?
# Are you checking a mathematical fact? Or are you testing whether subsequences
# satisfies a part of its specification? Or are you testing something else still?

We are actually testing only whether subsequences satisfies a part of a
fraction of specification. This kind of testing doesn't make much sense.

When testing powerComp we are assuming that the subsequence implementation is
correct. powerComp actually only tests if the subsequences function returns a list
with enough elements. It therefore does not test if the resulting list contains
all the needed sub sets.

If subsequences works accordingly we are testing the mathemetical fact that
a power set of a list with n elements contains 2 ^ n subsets against a finite
list of randomly generated variations of n.

-}
