{--
Time spent: 20m, after figuring out the basics inside exercise1 it was easy
to find a solution for this exercise. Most of the time was spent answering the
text based questions.
--}

module Exercise2 (test) where
import Test.QuickCheck
import Data.List

{-- Lab 1 - Exercise 2:
Task:
Redo exercise 4 of Workshop 1 by replacing sets by lists, and testing the
property for integer lists of the form [1..n].

Questions (1):
Is the property hard to test?
If you find that it is, can you given a reason why?

Answer (1):
It is expensive to test this property with large instances of n.
The runtime of the t4_subsequenceSize1 function is growing quadratic (O(n^2)).
subsequence must generate n^2 elements before the length function can access
the size of the generated list.

t4_subsequenceSize2 is much more elegant because it has a constant runtime (O(1))

Questions (2):
when you perform the test for exercise 4, what are you testing actually?
Are you checking a mathematical fact?
Or are you testing whether subsequences satisfies a part of its specification?
Or are you testing something else still?

Answer (2):
When testing t4_subsequenceSize1 I’m assuming that the subsequence
implementation is correct. t4 actually tests if the subsequences function
returns a list with enough elements. It therefore does not test if the
resulting list contains all the needed sub sets.
If subsequences works accordingly we are testing the mathemetical fact that
a power set of a list with n elements contains 2 ^ n subsets against a finite
list of randomly generated variations of n.

--}

t4_subsequenceSize1 :: Int -> Int
t4_subsequenceSize1 0 = 1
t4_subsequenceSize1 n = length ( subsequences [1..n]  )

t4_subsequenceSize2 :: Int -> Int
t4_subsequenceSize2 n = 2 ^ n

t4 :: Positive Int -> Bool
t4 (Positive n) = t4_subsequenceSize1 n == t4_subsequenceSize2 n

test :: IO()
test = verboseCheck t4
