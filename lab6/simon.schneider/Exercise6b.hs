module Exercise6b where

import Lecture6Dirty

{------------------------------------------------------------------------------
Time spent: 1h

TASK:
Find large mersenne numbers using Miller-Rabin

RECIPE:
1. Find prime p
2. Check if 2 ^ p - 1 is also prime
------------------------------------------------------------------------------}

isMersennePrime :: (Integer -> IO Bool) -> Integer -> IO Bool
isMersennePrime f n =
  do
    isP <- f 2
    isMp <- f (2 ^ n - 1)
    return (isP && isMp)

findMersennePrimes :: (Integer -> IO Bool) -> [Integer] -> IO [Integer]
findMersennePrimes f [] = return []
findMersennePrimes f (n:ns) =
  do
    isMp <- isMersennePrime f n
    if isMp then
      do
        putStrLn ("FOUND Mersenne prime candidate: " ++ show n)
        t <- findMersennePrimes f ns
        return (n : t)
    else
        findMersennePrimes f ns


isPrimeMonad :: Integer -> IO Bool
isPrimeMonad n = return (prime n)

findMersennePrimesMR :: Int -> [Integer] -> IO [Integer]
findMersennePrimesMR k ns = findMersennePrimes (primeMR k) ns

findMersennePrimesSafe :: [Integer] -> IO [Integer]
findMersennePrimesSafe ns = findMersennePrimes isPrimeMonad ns
