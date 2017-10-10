module Exercise6b where

-- | Exercise6b
-- | ===========================================================================
-- | Time spent: 30m

import Data.List
import System.Random
import Lecture6

-- | Generates a Mersenne number from p
mersen :: Integer -> Integer
mersen p = (2^p) - 1

-- | Returns an infinite list of Mersenne numbers (potential primes)
-- | by running "mersen" over the infinite list of primes.
potentialMersenPrimes :: [Integer]
potentialMersenPrimes = map (\x -> mersen x) primes

-- | Runs over the infinite list of potential Mersenne primes and checks them
-- | with Miller-Robin algorithm. Whenever the test pasees, returns the original
-- | prime that is the base for the found Mersenne prime.
findMersenPrimes :: Int -> Int -> IO ()
findMersenPrimes k i = do
  let e = potentialMersenPrimes!!i
  res <- primeMR k e -- | using primeMR from the Lecture6 code
  if res then
    print (primes!!i)
  else
    putChar '.'
  findMersenPrimes k (i+1)

-- | Report
-- | ===========================================================================
-- | My little function found the following resultis until it fell apart:
-- | *Exercise6b> findMersenPrimes 1 0
-- | 2
-- | 3
-- | 5
-- | 7
-- | 13
-- | 17
-- | 19
-- | 31
-- | 61
-- | 89
-- | 107
-- | 127
-- | 521
-- | 607
-- | 1279
-- | ...
-- |
-- | According to this article: https://primes.utm.edu/mersenne/, all of them
-- | are valid Mersenne numbers.
