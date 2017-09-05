module Lab1 where
import Data.List
import Test.QuickCheck

 -- Time
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

{-
counterexamples :: Int -> Integer --[([Integer],Integer)]
counterexamples n |
  otherwise = prime(product(take n primes) + 1) : counterexamples(n)

getNotPrimes :: Int -> [Integer]
getNotPrimes n = counterexamples not $ primes
-}
