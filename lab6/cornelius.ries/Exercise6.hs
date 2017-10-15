module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6
import Exercise5

-- Time spent:

mrTest :: Int -> [Integer] -> IO [Integer]
mrTest i (x:xs) =
  do
    b <- primeMR i x
    if b then
      do
        putStrLn ("false positive: " ++ show x)
        r <- mrTest i xs
        return (x : r)
    else
      mrTest i xs

-- no found false positives
-- TODO compare fermat vs carmichael

mprimesGen :: Int -> [Integer] -> IO [Integer]
mprimesGen i [] = return []
mprimesGen i (x:xs) =
  do
    b <- primeMR i (2^x-1)
    if b then
      do
        putStrLn ("mersenne prime: " ++ show (2^x-1))
        r <- mprimesGen i xs
        return (x : r)
    else
      mprimesGen i xs

{-

mprimesGen 1 (take 7 primes)

mersenne prime: 3
mersenne prime: 7
mersenne prime: 31
mersenne prime: 127
mersenne prime: 8191
mersenne prime: 131071

[2,3,5,7,13,17]

running the code with 1 more take gives a segfault (memory)

https://en.wikipedia.org/wiki/Mersenne_prime

according to the wikipedia page the generated marsenne primes are correct

-}
