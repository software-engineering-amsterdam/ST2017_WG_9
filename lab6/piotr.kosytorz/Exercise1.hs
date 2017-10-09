module Exercise1 where

import Data.List
import System.Random
import Math.NumberTheory.Logarithms
import Lecture6

-- integerLog2

-- | infinite list of powers of two
p2 = iterate (*2) 2

-- | subpower of 2 = length of the list = integerLog2

-- | x^33 mod 5 = (x^32 mod 5)*(x^1 mod 5)
-- | explanation:
-- | number of recurrences: integerLog2 33 = 5
-- | what is left: 33-(2^5) = 33-32 = 1

buildPow2 :: Integer -> Int -> Integer -> Integer
buildPow2 x i n
  | i > 0 = buildPow2 (x^2 `mod` n) (i-1) n
  | otherwise = x

myExM :: Integer -> Integer -> Integer -> Integer
myExM x y n = ((buildPow2 x (integerLog2 y) n) * (x^(y-2^(integerLog2 y)) `mod` n)) `mod` n
