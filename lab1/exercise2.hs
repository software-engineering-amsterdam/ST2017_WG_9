import Data.List
import Test.QuickCheck

{- We've agreed to use lists 1..n, which idicates that we work only with natural number -}
powerComp :: Positive Int -> Bool
powerComp (Positive n) = 2^n == length (subsequences [1..n])

test = quickCheckResult powerComp

{-

Answers to the questions:
=========================

# Is the property hard to test? If you find that it is, can you given a reason why?
It's hard to test it on a machine, as haskell is building powersets for different
test cases, which has a exponencial memory complexity ( O(2^n) ).
{-- TODO: Add that 2^n is not responsible for complexity, easy to write test - hard to execute --}


# Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?
> We are actually testing only whether subsequences satisfies a part of a fraction of specification. This kind of testing doesn't make much sense.

hen testing powerComp I’m assuming that the subsequence
implementation is correct. powerComp actually tests if the subsequences function
returns a list with enough elements. It therefore does not test if the
resulting list contains all the needed sub sets.
If subsequences works accordingly we are testing the mathemetical fact that
a power set of a list with n elements contains 2 ^ n subsets against a finite
list of randomly generated variations of n.

-}
