module Exercise3 (test1, test2, test3) where

import Data.List
import Test.QuickCheck

factorial :: Int -> Int
factorial n = product[1..n]
{-
Alternative implementation (no disadvanteges, but solution on top is shorter)
factorial 0 = 1
factorial n = n * (factorial (n-1))
-}

{- Number of distinctive permutations of a finite set of n elements = n! -}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

{-
Is the property hard to test? If you find that it is, can you given a reason why?

> The property is hard to test because the results are factorial N which will soon
be too much for the computer to calculate in a reasonable time.
Factorial time complexity, O(n!) for calculating all permutations.

-}
permutationsComp :: Positive Int -> Bool
permutationsComp (Positive n) = factorial n == length (permutations [1..n])

compareFunctionsUnordered :: Positive Int -> Bool
compareFunctionsUnordered (Positive n) = perms [1..n] == permutations [1..n]

compareFunctionsOrdered :: Positive Int -> Bool
compareFunctionsOrdered (Positive n) = sort (perms [1..n]) == sort (permutations [1..n])

{-
Again, give your thoughts on the following issue: when you perform the test for
exercise 5, what are you testing actually?
Are you checking a mathematical fact?
Or are you testing whether perms satisfies a part of its specification?
Or are you testing something else still?

test1 and test 2:
We're testing whether the permutations function from Data.List has the same
output as our own "perms" function when we call it with the same arguments.

We can see that test1 fails and test2 passes because "perms" sorts its elements
in a diffrent way

test2 passes (for small instances of n) because it only compairs if the resulting
elements are the same (not their order)

test3:
We are testing if the library function "permutations" produces the right amount
of elements but do not test if each of those elements contain the correct list
of numbers. We are therefore only testing a part of its specification.

We cannot ultimatly tell with QuickCheck if the "permutations" function or the
"perms" function are implemented correctly because we cannot test very large
instances of "n". (Maybe use Hoare calculus to prove correctness.)

Note: We are using verbose check to see which instances of n take "forever"
-}
test1 :: IO()
test1 = verboseCheck compareFunctionsOrdered

test2 :: IO()
test2 = verboseCheck compareFunctionsUnordered

test3 :: IO()
test3 = verboseCheck permutationsComp
