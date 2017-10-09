module Exercise1Alternative where

import Lecture6
import Test.QuickCheck

{------------------------------------------------------------------------------
time spent: 1h 30m, find effient algorithm, implement, test

Based on efficient algorithm presented in:
https://www.youtube.com/watch?v=sL-YtCqDS90

EXAMPLE:
5 ^ 40 mod 7 = ?
-------------------------------
1.) 5 ^ 1 mod 7 = 5
2.) 5 ^ 2 mod 7 = 5 ^ 2 mod 7 = 4
3.) 5 ^ 4 mod 7 = 4 ^ 2 mod 7 = 2
4.) 5 ^ 8 mod 7 = 2 ^ 2 mod 7 = 4
5.) 5 ^ 16 mod 7 = 4 ^ 2 mod 7 = 2
6.) 5 ^ 32 mod 7 = 2 ^ 2 mod 7 = 4
--> This is what getExMSteps does, getExMSteps 5 40 7
*Exercise1> getExMSteps 5 40 7
[(1,5),(2,4),(4,2),(8,4),(16,2),(32,4)]

Split y in powers of two:
*Exercise1> numberToPowerOfTwos 40
[32,8]

Calculate mod with steps from above:
(4 * 4) `mod` 7 = 2
 |   |--- Taken from 4.)
 |
 |------- Taken from 6.)

TEST:
*Exercise1> quickCheck test
+++ OK, passed 100 tests.
------------------------------------------------------------------------------}

exM' :: Integer -> Integer -> Integer -> Integer
exM' x y m = r1 `mod` m
  where
    ss = getExMSteps x y m
    ps = map (floor  . logBase2) (numberToPowerOfTwos y)
    r1 = foldl (\a p -> snd(ss !! p) * a) 1 ps

numberToPowerOfTwos :: Integer -> [Integer]
numberToPowerOfTwos 0 = []
numberToPowerOfTwos x = (p2) : numberToPowerOfTwos (x - p2)
  where p2 = 2 ^ floor(logBase2 x)

getExMSteps :: Integer -> Integer -> Integer -> [(Integer,Integer)]
getExMSteps x y m = getExMSteps' x y' m
  where y' = 2 ^ floor(logBase2 y)

getExMSteps' :: Integer -> Integer -> Integer -> [(Integer,Integer)]
getExMSteps' x 1 m = [(1, x `mod` m)]
getExMSteps' x y m = t ++ [(y,(p ^ 2) `mod` m)]
  where
    t = getExMSteps' x (y `div` 2) m
    p = snd (last t)


logBase2 :: Integer -> Float
logBase2 y = logBase 2 y'
  where
    y' = fromIntegral y :: Float

mod' :: Integer -> Integer -> Integer -> Integer
mod' x y m = x ^ y `mod` m

test :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
test (Positive x) (Positive y) (Positive m) = exM' x y m == x ^ y `mod` m && exM' x y m == exM x y m
