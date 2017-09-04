module Exercise3 where
import Test.QuickCheck
import Data.List

{--
Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the
property for integer lists of the form [1..n].

Questions (1):
Is the property hard to test? If you find that it is, can you given a reason why?

//TODO

Questions (2):
Again, give your thoughts on the following issue:
when you perform the test for exercise 5, what are you testing actually?
Are you checking a mathematical fact?
Or are you testing whether perms satisfies a part of its specification?
Or are you testing something else still?

//TODO

--}

{--Taken from: Workshop 1, adjusted for simplicity --}
perms :: [Int] ->[[Int]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
 insrt _ [] = [[x]]
 insrt _ (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

permutationsLength :: [Int] -> Int
permutationsLength x = factorial ( length x )

{-- Test for own approach (perms function): --}

comparePermsLength :: Int -> Bool
comparePermsLength x = length (perms [1..x]) == permutationsLength [1..x]

testPermsLength :: IO ()
testPermsLength = quickCheck comparePermsLength

{-- Test for standard library approach (permutations function): --}
comparePermutationsLength :: Int -> Bool
comparePermutationsLength x = length (permutations [1..x]) == permutationsLength [1..x]

testPermutationsLength :: IO()
testPermutationsLength = quickCheck comparePermutationsLength


{-- Bonus: Compare own implementation with standard library approach --}
{-- Will fail because the subsets are sorted differently --}
comparePermutations1 :: Int -> Bool
comparePermutations1 x = perms [1..x] == permutations [1..x]
{--TODO: Write compair function that ignores order--}
