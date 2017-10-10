module Exercise6b where

import Lecture6Dirty

{------------------------------------------------------------------------------
Time spent: 1h

TASK:
Find large mersenne numbers using Miller-Rabin

RECIPE:
1. Find prime p
2. Check if 2 ^ p - 1 is also prime

TESTS:
findMersennePrimesMR 5  [2..5000]
[2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217,4253]
(77.20 secs, 31,045,459,864 bytes)

*Exercise6b> findMersennePrimesMR 1 [2..5000]
[2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217,4253]
(77.85 secs, 30,490,365,264 bytes)
--> k does not make a large diffrence in time and the quality.



*Exercise6b> findMersennePrimesSafe primes
FOUND Mersenne prime candidate: 2
FOUND Mersenne prime candidate: 3
FOUND Mersenne prime candidate: 5
FOUND Mersenne prime candidate: 7
FOUND Mersenne prime candidate: 13
FOUND Mersenne prime candidate: 17
FOUND Mersenne prime candidate: 19
FOUND Mersenne prime candidate: 31
--> Does not find next mersenne number (exponent 61) in reasonable time

*Exercise6b> findMersennePrimesMR 1 primes
FOUND Mersenne prime candidate: 2
FOUND Mersenne prime candidate: 3
FOUND Mersenne prime candidate: 5
FOUND Mersenne prime candidate: 7
FOUND Mersenne prime candidate: 13
FOUND Mersenne prime candidate: 17
FOUND Mersenne prime candidate: 19
FOUND Mersenne prime candidate: 31
FOUND Mersenne prime candidate: 61
FOUND Mersenne prime candidate: 89
FOUND Mersenne prime candidate: 107
FOUND Mersenne prime candidate: 127
FOUND Mersenne prime candidate: 521
FOUND Mersenne prime candidate: 607
FOUND Mersenne prime candidate: 1279
FOUND Mersenne prime candidate: 2203
FOUND Mersenne prime candidate: 2281
FOUND Mersenne prime candidate: 3217
FOUND Mersenne prime candidate: 4253
FOUND Mersenne prime candidate: 4423

A faster approach than checking all natural numbers is checking the prime numbers. (see above)

Comparing the list with https://primes.utm.edu/mersenne/ shows that the numbers found are actually
mersenne primes. We can also see that our little Haskell program and a new MacBook Pro can outsmart
a super computer from 1971 ;)

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
