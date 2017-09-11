module Exercise4 where

{--
time: 1h 30m

A permutation of a finite list is another finite list with the same elements, b
ut possibly in a different order. For example, [3,2,1] is a permutation of
[1,2,3], but [2,2,0] is not. Write a function

Test cases example:
*Exercise4 Data.List> quickCheck testSame
+++ OK, passed 100 tests.
*Exercise4 Data.List> quickCheck testSortedL
+++ OK, passed 100 tests.
*Exercise4 Data.List> quickCheck testSortedR
+++ OK, passed 100 tests.
*Exercise4 Data.List> quickCheck testReversedL
+++ OK, passed 100 tests.
*Exercise4 Data.List> quickCheck testReversedR
+++ OK, passed 100 tests.

Example test cases for shuffling lists:
*Exercise4 Data.List> testShuffle [1,2,6,7,8,5,4,8,4,4, (-1)]
True
*Exercise4 Data.List> testShuffle "Hallo Welt nicht in englisch"
True
*Exercise4 Data.List> testShuffle "Hello world in english"
True
-}

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad
import Test.QuickCheck

{------------------------------------------------------------------------------
              Check if two lists are permutations of each other
-------------------------------------------------------------------------------}

{-
isPermutation: Checks if two lists are equal (including amount of duplicates)
but ignores their order.

Idea behind implementation: Shrink the lists from the left side and remove one
element on both lists if it is part of both lists. If both lists are empty at
the end we can assume that n equal elements where removed from list 1 and list2.

-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation l1@(x:xs) l2@(y:ys)
  | x == y = isPermutation xs ys
  | x `elem` ys = isPermutation xs (removeOneFromList x l2)
  | y `elem` xs = isPermutation ys (removeOneFromList y l1)
  | otherwise = False

{-
removeOneFromList: Removes the first occasion of an element a from the list l
if a is not element of l it will return l.
After implementing I found out that there is also a "delete" function that does
the same job. I still like to keep this as another example of recursion.
-}
removeOneFromList :: Eq a => a -> [a] -> [a]
removeOneFromList a l = _removeOneFromList a [] l

{-
_removeOneFromList: Removes the first occasion of an element a from the list l
if a is not element of l it will return l.

It needs a start list, usually empty to keep track of the elements that were
already checked. The basic idea behind this is a pointer that wanders from the
left to the right.
-}
_removeOneFromList :: Eq a => a -> [a] -> [a] -> [a]
_removeOneFromList _ s [] = s
_removeOneFromList a s (y:ys)
  | a == y = s ++ ys
  | otherwise = _removeOneFromList a (s ++ [y]) ys

{------------------------------------------------------------------------------
                                  Testing
-------------------------------------------------------------------------------}

testSame :: Eq a => [a] -> Bool
testSame l = isPermutation l l

testSortedL :: Ord a => [a] -> Bool
testSortedL l = isPermutation (sort l) l

testSortedR :: Ord a => [a] -> Bool
testSortedR l = isPermutation l (sort l)

testReversedL :: Eq a => [a] -> Bool
testReversedL l = isPermutation (reverse l) l

testReversedR :: Ord a => [a] -> Bool
testReversedR l = isPermutation l (reverse l)

{-
shufle: Randomly__shuffles the position of each list element
Taken from https://wiki.haskell.org/Random__shuffle
-}

_shuffle :: [a] -> IO [a]
_shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

{-
testShuffle: Tests if shuffled lists are still permutations.
-}
testShuffle :: Eq a => [a] -> IO Bool
testShuffle l = do
   s <- _shuffle l
   return (isPermutation s l)
