module Lab4ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Lab4ex2

-- Time Spent
-- 20m implementation
-- 30m on tester and first tests

-- unionSet is already defined in SetOrd ?

diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet (Set []) _ = emptySet
diffSet (Set (x:xs)) set2
  | not(inSet x set2) = insertSet x (diffSet (Set xs) set2)
  | otherwise = diffSet (Set xs) set2

interSet :: (Ord a) => Set a -> Set a -> Set a
interSet (Set []) _ = emptySet
interSet (Set (x:xs)) set2
  | inSet x set2 = insertSet x (interSet (Set xs) set2)
  | otherwise = interSet (Set xs) set2

testSet1 :: Set Int
testSet1 = insertSet 1 (insertSet 3 emptySet)

testSet2 :: Set Int
testSet2 = insertSet 1 (insertSet 4 emptySet)

---- Tester for own generator ----

setTester :: [Set Int] -> [Set Int] -> (Set Int -> Set Int -> Bool) -> IO ()
setTester [] [] _ = putStrLn "All tests passed"
setTester (f:fs) (g:gs) p = do
  setTester fs gs p
  if p f g then
    putStrLn "Test passed"
  else
    putStrLn ("Test failed: " ++ show f)

testSetPredicate :: Int -> (Set Int -> Set Int -> Bool) -> IO ()
testSetPredicate n p = do
  fs <- generateTestSets n
  gs <- generateTestSets n
  (setTester fs gs p)

---- Tester for own generator ----

-- commutativity Tests
unionSetCom :: Set Int -> Set Int -> Bool
unionSetCom a b = unionSet a b == unionSet b a

diffSetCom :: Set Int -> Set Int -> Bool
diffSetCom a b = diffSet a b /= diffSet b a

interSetCom:: Set Int -> Set Int -> Bool
interSetCom a b = interSet a b == interSet b a

-- test the union Set method
-- set a should be in union of ab
-- set b should be in union of ab
unionSetP :: Set Int -> Set Int -> Bool
unionSetP a b = subSet a (unionSet a b) && subSet b (unionSet a b)

-- test the diff set
-- if diff is emtpy and subset a b
-- or
-- diff of a b is in a and diff a b is NOT in b
diffSetP :: Set Int -> Set Int -> Bool
diffSetP a b = isEmpty(diffSet a b) && subSet a b || subSet (diffSet a b) a && not ( subSet (diffSet a b) b )


testAllP :: Set Int -> Set Int -> Bool
testAllP a b = all (\f -> f a b) [unionSetCom, diffSetCom, interSetCom, unionSetP, diffSetP]

testAll :: IO ()
testAll = quickCheck testAllP
