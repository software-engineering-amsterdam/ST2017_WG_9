-- taken from https://stackoverflow.com/questions/4826630/type-class-problem-concerning-flexibleinstances
{-# LANGUAGE FlexibleInstances #-}

module Lab4ex2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Time Spent: 45m

-- Random Generator

getRandomInt :: Int -> IO Int
getRandomInt u = getStdRandom (randomR (0,u))

addRandomSetElements :: Int -> Set Int -> IO (Set Int)
addRandomSetElements n s =
  if n > 0
    then do
      m <- getRandomInt 1000
      addRandomSetElements (n-1) (insertSet m s)
  else return s

genSetM :: IO (Set Int)
genSetM =
  do
    m <- getRandomInt 100
    addRandomSetElements m emptySet

generateTestSets :: Int -> IO [Set Int]
generateTestSets 0 = return []
generateTestSets j = do
  h <- genSetM
  t <- generateTestSets (j - 1)
  return (h:t)

-- QuickCheck definitions

-- set default generator for type Set Int
instance Arbitrary (Set Int) where
    arbitrary = genSet

-- default generator to generate Set Int
genSet :: Gen (Set Int)
genSet =
      do
        size <- elements [1..100]
        genSetS size emptySet

genSetS :: Int -> Set Int -> Gen (Set Int)
genSetS n s =
  if n > 0
    then do
      m <- elements [1..1000]
      genSetS (n-1) (insertSet m s)
  else return s
