module Ex4 (reversal, prime, primes, reversals, reversalComp) where

import Data.List
import Test.QuickCheck

{--
  Author: Piotr kosytorz
  Time spent: 30m
--}

{- The following functions were given in the assigment -}

reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

{- prime.reversal does the trick - it's a product of two properties -}
reversals = takeWhile(<10000)  (filter (prime.reversal) primes)

{-
  # Testing the reversal function:
  > From the data given in the excersise, I assume that we limit the computation
  > only to natural numbers (>= 0), therefore: Positive
-}

reversalComp :: Positive Integer -> Bool
reversalComp (Positive n) = n == reversal (reversal n);

test = verboseCheckResult (reversalComp)

{-
  reversal will fail in the following cases:
  - when the given number is divsible by any power of 10, because
    then it's reversal will not include the 0 at the beginning of
    the number. Example: 120 -> 021 = 21 -> 12
-}
