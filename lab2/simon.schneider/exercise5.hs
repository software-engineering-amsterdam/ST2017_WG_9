module Exercise5 where

import Data.List
import Test.QuickCheck

{--

Assumptions: Two empty lists are a permutation of each other and also a

*Exercise5 Data.List> quickCheck testLength
+++ OK, passed 100 tests.
*Exercise5 Data.List> quickCheck testSame
+++ OK, passed 100 tests.
*Exercise5 Data.List> quickCheck testSame2
+++ OK, passed 100 tests.
*Exercise5 Data.List> quickCheck testSameElements
+++ OK, passed 100 tests.
*Exercise5 Data.List> quickCheck testSwappedArguments
+++ OK, passed 100 tests.
*Exercise5 Data.List> quickCheck testIsAlsoPermutation
+++ OK, passed 100 tests.
*Exercise5 Data.List> quickCheck testDoNotShareElements
+++ OK, passed 100 tests.


//TODO: Provide an ordered list of properties by strength using the weakear and stronger definitions.

--}

{------------------------------------------------------------------------------
                          Functions given by exercise
-------------------------------------------------------------------------------}

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

{-

A derangement of the list [0..n-1] of natural numbers is a permutation ππ of
 the list with the property that for no xx in the list π(x)=xπ(x)=x.

-}

isDerangement :: [Int] -> [Int] -> Bool
isDerangement l1 l2= noElementsSharedAtSamePosition l1 l2 && isPermutation l1 l2

noElementsSharedAtSamePosition :: [Int] -> [Int] -> Bool
noElementsSharedAtSamePosition (x:xs) (y:ys)
  | x == y = False
  | otherwise = noElementsSharedAtSamePosition xs ys
noElementsSharedAtSamePosition _ _ = True

{-Taken from exercise 4-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation l1@(x:xs) l2@(y:ys)
  | x == y = isPermutation xs ys
  | x `elem` ys = isPermutation xs (delete x l2)
  | y `elem` xs = isPermutation ys (delete y l1)
  | otherwise = False

deran :: [Int] -> [[Int]]
deran l = [x | x <- permutations l, noElementsSharedAtSamePosition l x]

{-
Properties for isDerangement to test:
- Always False for list that do not have the same size
- Always False for lists that are the same
- Always False for lists that do not have the same elements
- Always False for lists that are not a permutation of each other
- Always True for dearanged lists
-}

sameLengthPredicate :: ([Int],[Int]) -> Bool
sameLengthPredicate (l1,l2) = length l1 == length l2

notSameListsPredicate :: ([Int],[Int]) -> Bool
notSameListsPredicate (l1,l2) = l1 /= l2 || (null l1 && null l2)

permutationPredicate :: ([Int],[Int]) -> Bool
permutationPredicate (l1,l2) = isPermutation l1 l2

testLength :: [Int] -> [Int] -> Bool
testLength l1 l2 = isDerangement l1 l2 --> sameLengthPredicate (l1,l2)

testSame :: [Int] -> Bool
testSame l = isDerangement l l --> null l

testSame2 :: [Int] -> [Int] -> Bool
testSame2 l1 l2 = isDerangement l1 l2 --> notSameListsPredicate (l1,l2)

testSameElements :: [Int] -> [Int] -> Bool
testSameElements l1 l2 = isDerangement l1 l2 --> sort l1 == sort l2

testSwappedArguments :: [Int] -> [Int] -> Bool
testSwappedArguments l1 l2 = isDerangement l1 l2 --> isDerangement l2 l1

testIsAlsoPermutation :: [Int] -> [Int] -> Bool
testIsAlsoPermutation l1 l2 = isDerangement l1 l2 --> isPermutation l1 l2

testDoNotShareElementsAtSamePosition :: [Int] -> [Int] -> Bool
testDoNotShareElementsAtSamePosition l1 l2 = isDerangement l1 l2 --> noElementsSharedAtSamePosition l1 l2


(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)

powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
                where xss = powerset xs

generateTestLists :: Int -> [[Int]]
generateTestLists n = foldl (\a x -> a ++ permutations [1..x]) [] [1..n]

makeTuplesList :: [Int] -> [[Int]] -> [([Int], [Int])]
makeTuplesList l1 ll = map (\l2 -> (l1,l2)) ll

generateTestTupels :: [[Int]] -> [([Int], [Int])]
generateTestTupels l = foldl (\a x -> a ++ makeTuplesList x l) [] l

testDomain = generateTestTupels(generateTestLists 3)
