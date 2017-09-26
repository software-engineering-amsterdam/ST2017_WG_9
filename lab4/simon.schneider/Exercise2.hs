module Exercise2 where

{------------------------------------------------------------------------------
Time spent: 1h 30m (Implementation + Reusable tester functions that I could
also have used for Lab3 - Forms)
-------------------------------------------------------------------------------}

import Data.List
import System.Random
import Lecture2(getRandomInt)
import SetOrd
import Test.QuickCheck

exampleSet :: Set Int
exampleSet = list2set [1,2,4,3,4]


getRandomIntBound :: Int -> Int -> IO Int
getRandomIntBound l u = getStdRandom (randomR (l,u))

{------------------------------------------------------------------------------
                      Own generic helper functions
-------------------------------------------------------------------------------}

tester' :: (Show a) => [a] -> (a -> Bool) -> IO ()
tester' [] _ = putStrLn "All tests passed"
tester' (f:fs) p =
  if p f then
    do
      putStrLn ("Test passed: " ++ show f)
      tester' fs p
  else
    putStrLn ("Test failed: " ++ show f)

rtester' :: (Show a) => IO [a] -> (a -> Bool) -> IO ()
rtester' ml p = do
  l <- ml
  tester' l p

generateMany :: Int -> IO a -> IO [a]
generateMany 0 _ = return []
generateMany n g = do
  t <- generateMany (n - 1) g
  h <- g
  return (h : t)

{------------------------------------------------------------------------------
                    Implementation from scratch
-------------------------------------------------------------------------------}


getRandomIntNotInList :: Int -> [Int] -> IO Int
getRandomIntNotInList u l
  | length l >= u = error "List is already full, cannot get random number with given upper bound."
  | otherwise = do
  r <- getRandomInt u
  if r `elem` l
    then
      getRandomIntNotInList u l
    else
      return r

getRandomDistinctList :: Int -> Int -> IO [Int]
getRandomDistinctList _ 0 = return []
getRandomDistinctList u n
  | n > u = error "Cannot generate distinct list with length l if upper bound of random numbers is too low"
  | otherwise = do
    t <- getRandomDistinctList u (n-1)
    r <- getRandomIntNotInList u t
    return (r : t)

getRandomSet' :: Int -> Int -> IO (Set Int)
getRandomSet' u n = do
  xs <- getRandomDistinctList u n
  return (list2set xs)

getRandomSet :: IO (Set Int)
getRandomSet = do
  n <- getRandomInt 50
  u <- getRandomIntBound n (n * 5)
  getRandomSet' u n

getRandomSetPair :: IO (Set Int, Set Int)
getRandomSetPair =
  do
    l <- getRandomSet
    r <- getRandomSet
    return (l,r)

setTester :: Int -> (Set Int -> Bool) -> IO()
setTester n p = rtester' (generateMany n getRandomSet) p

setPairTester :: Int -> ((Set Int, Set Int) -> Bool) -> IO()
setPairTester n p = rtester' (generateMany n getRandomSetPair) p

{------------------------------------------------------------------------------
                    Implement with QuickCheck

See: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
"Test Data Generators: The Type Gen" and "Class Arbitrary"

Benefits: QuickCheck is more flexible than the setTester and the setPairTester
from above, it also works with functions that take lists of sets with chars (see runQc3)
-------------------------------------------------------------------------------}
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
            xs <- arbitrary
            return (list2set xs)

randomIntSetGenerator :: Gen (Set Int)
randomIntSetGenerator = do
    list <- listOf arbitrary
    return (list2set list)

randomIntSets :: IO (Set Int)
randomIntSets = generate (randomIntSetGenerator::Gen (Set Int))

qcFunc1 :: Set Int -> Bool
qcFunc1 _ = True

qcFunc2 :: (Set Int,Set Int) -> Bool
qcFunc2 _ = True

qcFunc3 :: [Set Char] -> Bool
qcFunc3 _ = True

runQc1,runQc2,runQc3 :: IO()
runQc1 = verboseCheck qcFunc1
runQc2 = verboseCheck qcFunc2
runQc3 = verboseCheck qcFunc3
