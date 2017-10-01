module Exercise3 where

import Exercise2 (setPairTester)
import Data.List
import SetOrd

{--
Time spent: 1h (short implementation, thinking of tests for longer)
--}

setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (xs `intersect` ys)

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = list2set (xs ++ ys)

setDiffrence :: (Ord a) => Set a -> Set a -> Set a
setDiffrence (Set xs) (Set ys) = list2set (xs \\ ys)

getSetObjects :: (Ord a) => Set a -> [a]
getSetObjects (Set x) = x

{------------------------------------------------------------------------------
                    Test implementation

1. a union b contains all the elements from a and b
2. (a intersect b) + (a diff b) + (b diff a) = a union b
3. (a diff b) + (a intersect b) = a
4. (b diff a) + (a intersect b) = b
-------------------------------------------------------------------------------}

exampleSet1 :: Set Int
exampleSet1 = list2set [1,2,4,8]

exampleSet2 :: Set Int
exampleSet2 = list2set [1,2,5,6,7]

prop1 :: Ord a => (Set a, Set a) -> Bool
prop1 (s1@(Set xs), s2@(Set ys)) = all (`elem` us) xs && all (`elem` us) ys
  where
    u = setUnion s1 s2
    us = getSetObjects u

prop2 :: Ord a => (Set a, Set a) -> Bool
prop2 (x,y) = setUnion (setDiffrence x y) (setUnion (setDiffrence x y) (setIntersection x y)) == x

prop3 :: Ord a => (Set a, Set a) -> Bool
prop3 (x,y) = setUnion (setDiffrence x y) (setIntersection x y) == x

prop4 :: Ord a => (Set a, Set a) -> Bool
prop4 (x,y) = setUnion (setDiffrence y x) (setIntersection x y) == y

testProp1 = setPairTester 100 prop1
testProp2 = setPairTester 100 prop2
testProp3 = setPairTester 100 prop3
testProp4 = setPairTester 100 prop4
