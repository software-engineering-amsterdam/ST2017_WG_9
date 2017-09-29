module Exercise2 where

-- | Exercise2
-- | ===========================================================================
-- | Time spent: 2h

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- | Random Set generator from scratch
-- | ===========================================================================
-- | This is a little bit problematic, as the randomness can be defined
-- | on multiple levels:
-- |   - set size
-- |   - sign of elements in the set
-- |   - range of possible elements in the set, etc
-- | Therefore I will go for the following border values:
-- | Set size: [0, 100]
-- | Value of a single element in the set: [-1000, 1000]

-- | returns a random set size
randomSize :: IO Int
randomSize = randomRIO (1,100)

-- | returns an infinite list of random numbers between -1000 and 1000
randomInfiniteList :: IO [Integer]
randomInfiniteList = do
  actualList <- newStdGen
  return $ randomRs (-1000,1000) actualList

-- | returns a list of random size and random elements
randomListRandomSize :: IO [Integer]
randomListRandomSize = do
  xs <- randomInfiniteList
  n <- randomSize
  return $ take n xs

-- | removes duplicates:  sort, group, take only the first element of each group
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

-- | generates a random set
randomSet :: IO (Set Integer)
randomSet = do
  list <- randomListRandomSize
  let uList = removeDuplicates list
  return $ list2set uList

-- | A QuickCheck generator for random sets
-- | ===========================================================================

-- | instance of arbitrary for Set a
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = fmap list2set arbitrary

-- | a property for testing Set Int sets
prop_RandomIntSet :: Set Int -> Bool
prop_RandomIntSet _ = True

-- | testing with verboseCheck (yields tested values)
test_RandomIntSet = verboseCheck prop_RandomIntSet

{--
  Chunk of the output:

  Passed:
  { -73,-70,-68,-67,-66,-65,-64,-59,-55,-52,-50,-45,-42,-40,-37,-36,-34,-29,-28,-26,-24,-22,-20,-19,-9,0,4,5,6,7,8,9,12,13,14,15,21,22,24,26,32,34,35,39,41,46,47,51,52,56,58,59,65,66,68,74 }
  Passed:
  { -67 }
  Passed:
  { -74,-68,-58,-55,-53,-48,-42,-41,-38,-28,-24,-19,-18,-15,-11,-9,1,4,6,10,14,19,22,24,31,42,44,45,47,60,76,78}
  Passed:
  { -76,-72,-70,-68,-67,-65,-64,-63,-61,-60,-54,-52,-51,-50,-47,-46,-34,-33,-31,-28,-25,-21,-16,-14,-11,-9,5,10,11,12,13,16,20,23,26,29,30,34,35,36,38,45,61,63,66,67,75,76,79}
  Passed:
  { -80,-78,-75,-69,-65,-61,-59,-56,-55,-50,-46,-44,-42,-36,-33,-30,-29,-26,-23,-22,-21,-19,-18,-16,-14,-10,-9,-7,-6,-5,-3,0,1,7,10,17,20,24,25,29,31,36,42,46,48,49,55,56,62,63,67,68,78}
  Passed:
  { -25,-13,-8,6,42,44,67}
  Passed:
  { -76,-65,-34,-28,-22,-14,-13,-9,1,6,9,10,13,17,20,23,35,36,42,46,48,61,71,77}
  Passed:
  { -83,-81,-66,-52,-49,-46,-43,-42,-36,-34,-29,-27,-25,-23,-21,-19,-18,-14,2,10,16,17,18,20,22,24,26,27,33,34,35,37,41,42,49,50,51,52,62,67,69,74,76,77,78}
  Passed:
  { -59,-4,20,36}

--}
