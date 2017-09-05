import Data.List
import Test.QuickCheck

{- The following functions were given in the assigment -}

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

{-
  I use here the "needle in the haystack" approach, which basically means
  that we take the whole set (haystack), and look in it for a subset that
  meets the given criteria: is a set of 101 consecutive primes which sum
  is also a prime.
-}

findVectorInHaystack :: [Integer] -> [Integer]
findVectorInHaystack haystack
   | prime (sum (take 101 haystack)) = take 101 haystack
   | otherwise = findVectorInHaystack (tail haystack)

solution = findVectorInHaystack primes
sumOfSolution = sum solution

{-
  This is a simple pro-forma test. It doesn't make sense to test this funtion,
  as it's implementation would be the same as the method itself.
-}
test = prime $ sumOfSolution
