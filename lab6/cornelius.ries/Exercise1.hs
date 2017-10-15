module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck

-- Time Spent: 3h

exM2 :: Integer -> Integer -> Integer -> Integer
exM2 x y n = (s * h) `mod` n
  where t = floor (logBase (fromIntegral 2) (fromIntegral y))
        r = y - 2^t
        s = exMHelper x t n
        h = exMHelper2 x 1 r n

exM3 :: Integer -> Integer -> Integer -> Integer
exM3 _ 0 _ = 1
exM3 x y n = (s * (exM2 x r n)) `mod` n
  where t = floor (logBase (fromIntegral 2) (fromIntegral y))
        r = y - 2^t
        s = exMHelper x t n

exMHelper :: Integer -> Integer -> Integer -> Integer
exMHelper s t n =
  if t == 0 then s
  else exMHelper (s^2 `mod` n) (t-1) n

exMHelper2 :: Integer -> Integer -> Integer -> Integer -> Integer
exMHelper2 x s t n =
  if t == 0 then s
  else exMHelper2 x ((x `mod` n) * s) (t-1) n
