module Exercise2 where

{------------------------------------------------------------------------------
Time spent: 1h 30m (Implementation + Reusable tester functions that I could
also have used for Lab3 - Forms)
-------------------------------------------------------------------------------}

import System.Random
import Lecture2(getRandomInt)
import SetOrd

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
                    Implementat with QuickCheck (TODO)
-------------------------------------------------------------------------------}
