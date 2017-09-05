module Lab1 where
import Data.List
import Test.QuickCheck

 -- Time 1h

reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

isReversalPrime :: Integer -> Bool
isReversalPrime n = prime(reversal(n))

reversalPrimes :: [Integer]
reversalPrimes = takeWhile(<10000) (filter isReversalPrime primes)
