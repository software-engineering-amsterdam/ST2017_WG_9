module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import Exercise1
import Lecture6

-- Time spent: 1h

testProp :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
testProp (Positive a) (Positive b) (Positive c) = exM2 a b c == expM a b c

-- function for executing the
bench :: Integer -> Integer -> Integer -> Integer -> (Integer -> Integer -> Integer -> Integer) -> Integer -> IO String
bench 0 _ _ _ _ b = return ("Done" ++ show b)
bench t xM yM nM f b =
    do
      x <- randomRIO (1,xM)
      y <- randomRIO (1,yM)
      n <- randomRIO (1,nM)
      let q = f x y n
      bench (t-1) xM yM nM f (q+b)

-- using :set +s in ghci to do benchmarking
