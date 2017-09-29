module Exercise3 where

-- | Exercise3
-- | ===========================================================================
-- | Time spent: 2h

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise2
import Library

-- | A ∪ B
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set $ a++b
-- Note: unionSet is already implemented in SetOrd.hs

-- | A \ B
setDiff :: (Ord a) => Set a -> Set a -> Set a
setDiff (Set a) (Set b) = list2set $ a \\ b

-- | A ∩ B = (A ∪ B) \ ((A \ B) ∪ (B \ A))
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection a b = setDiff (setUnion a b) (setUnion (setDiff a b) (setDiff b a))

-- | Testing
-- | ===========================================================================
-- | Own generator tests

-- | a property for testing a union of sets
-- | pass iff: A ⊆ (A ∪ B) && B ⊆ (A ∪ B) && (A ∪ B) \ A \ B = ∅
prop_own_gen_RandomUnionSet :: IO (Set Integer) -> IO (Set Integer) -> IO (Bool)
prop_own_gen_RandomUnionSet ma mb = do
  a <- ma
  b <- mb
  return (
    subSet a (setUnion a b) &&
    subSet b (setUnion a b) &&
    isEmpty (setDiff (setDiff (setUnion a b) a) b )
    )

test_own_gen_RandomUnionSet :: IO (Bool)
test_own_gen_RandomUnionSet = prop_own_gen_RandomUnionSet randomSet randomSet
{-- Result:
  *Exercise3> test_own_gen_RandomUnionSet
  True
--}

-- | a property for testing a difference of sets
-- | pass iff: (A ⊆ B && A \ B = ∅) || (A \ B ⊆ A && A \ B ⊈ B)
prop_own_gen_RandomDiffSet :: IO (Set Integer) -> IO (Set Integer) -> IO (Bool)
prop_own_gen_RandomDiffSet ma mb = do
 a <- ma
 b <- mb
 return (
   (
     isEmpty (setDiff a b) &&
     subSet a b
   )
   ||
   (
     subSet (setDiff a b) a &&
     not ( subSet (setDiff a b) b )
   )
   )

test_own_gen_RandomDiffSet :: IO (Bool)
test_own_gen_RandomDiffSet = prop_own_gen_RandomDiffSet randomSet randomSet
{-- Result:
 *Exercise3> test_own_gen_RandomDiffSet
 True
--}

-- | a property for testing a intersection of sets
-- | pass iff: A ∩ B ⊆ A && A ∩ B ⊆ B && (A ∩ B) ∪ A = A && (A ∩ B) ∪ A = B
prop_own_gen_RandomIntersectionSet :: IO (Set Integer) -> IO (Set Integer) -> IO (Bool)
prop_own_gen_RandomIntersectionSet ma mb = do
  a <- ma
  b <- mb
  return (
    subSet (setIntersection a b) a &&
    subSet (setIntersection a b) b &&
    setUnion (setIntersection a b) a == a &&
    setUnion (setIntersection a b) b == b
    )

test_own_gen_RandomIntersectionSet :: IO (Bool)
test_own_gen_RandomIntersectionSet = prop_own_gen_RandomIntersectionSet randomSet randomSet
{-- Result:
  *Exercise3> test_own_gen_RandomIntersectionSet
  True
--}

-- | Testing
-- | ===========================================================================
-- | QuickCheck

-- | a property for testing a union of sets
-- | pass iff: A ⊆ (A ∪ B) && B ⊆ (A ∪ B) && (A ∪ B) \ A \ B = ∅
prop_RandomUnionSet :: Set Int -> Set Int -> Bool
prop_RandomUnionSet a b =
  subSet a (setUnion a b) &&
  subSet b (setUnion a b) &&
  isEmpty (setDiff (setDiff (setUnion a b) a) b )

-- | a property for testing a difference of sets
-- | pass iff: (A ⊆ B && A \ B = ∅) || (A \ B ⊆ A && A \ B ⊈ B)
prop_RandomDiffSet :: Set Int -> Set Int -> Bool
prop_RandomDiffSet a b =
  (
    isEmpty (setDiff a b) &&
    subSet a b
  )
  ||
  (
    subSet (setDiff a b) a &&
    not ( subSet (setDiff a b) b )
  )

-- | a property for testing a intersection of sets
-- | pass iff: A ∩ B ⊆ A && A ∩ B ⊆ B && (A ∩ B) ∪ A = A && (A ∩ B) ∪ A = B
prop_RandomIntersectionSet :: Set Int -> Set Int -> Bool
prop_RandomIntersectionSet a b =
  subSet (setIntersection a b) a &&
  subSet (setIntersection a b) b &&
  setUnion (setIntersection a b) a == a &&
  setUnion (setIntersection a b) b == b

-- | testing with quickCheck
test_randomUnionSet = quickCheck prop_RandomUnionSet
test_RandomDiffSet = quickCheck prop_RandomDiffSet
test_RandomIntersectionSet = quickCheck prop_RandomIntersectionSet

{--
  Results:

  *Exercise3> test_randomUnionSet
  +++ OK, passed 100 tests.
  *Exercise3> test_RandomDiffSet
  +++ OK, passed 100 tests.
  *Exercise3> test_RandomIntersectionSet
  +++ OK, passed 100 tests.
--}
