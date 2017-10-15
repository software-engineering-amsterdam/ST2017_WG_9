module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6
import Exercise3

-- Time spent: 1h

smallestF :: Int -> [Integer] -> IO [Integer]
smallestF i (x:xs) =
  do
    b <- primeTestsF i x
    if b then
      do
        putStrLn ("false positive: " ++ show x)
        r <- smallestF i xs
        return (x : r)
    else
      smallestF i xs

smallestF2 :: Integer -> [Integer]
smallestF2 i = filter (\x -> exM i (x-1) x == 1) composites

smallestF3 :: Integer -> [Integer]
smallestF3 i = filter (\x -> bl x) composites
  where bl x = all (\a -> exM a (x-1) x == 1) [1..i]

-- if you increase k you get less false positives
