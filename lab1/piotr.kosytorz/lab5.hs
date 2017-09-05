import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

doTheWork :: [Integer] -> [Integer]
doTheWork numbers
   | prime (sum (take 101 numbers)) = take 101 numbers
   | otherwise = doTheWork (tail numbers)
