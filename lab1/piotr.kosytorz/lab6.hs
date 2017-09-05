import Data.List
import Test.QuickCheck

{- The following functions were given in the assigment -}

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

{- Primes generator starting from 2 -}
primes :: [Integer]
primes = 2 : filter prime [3..]

{-
  We're looking for sets of consecutive prime numbers (p1,...,pn) within primes
  that refute the following conjecture:
  (p1×⋯×pn)+1 is also prime.

  Notice:
  From the excersise I assume, that we are always looking for products of the
  consecutive primes starting from 2, so 2,3,5,7,11,13,...
  I'm assuming by this, that the theorem refers only to consecutive sets of
  prime numbers starting from 2
-}

{-
  Checks if the vector of n consecutive primes fails the theorem and returns
  a touple ([Integer], Integer) with the found set and its (p1×⋯×pn)+1 value
-}
findCounterexampleVectorInPrimes :: Int -> ([Integer], Integer)
findCounterexampleVectorInPrimes n
   | not $ prime ((product (take n primes))+1) = (take n primes, ((product (take n primes))+1))
   | otherwise = ([], 0)

{-
  Lazy function: generates counterexamples.
  Notice: The filter is to sort out the empty touples
-}
counterexamples = filter (\m -> m /= ([],0)) $ map(\n -> findCounterexampleVectorInPrimes n)[2..]
