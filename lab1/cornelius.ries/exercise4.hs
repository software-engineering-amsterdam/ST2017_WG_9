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

-- Test

reversalTest :: Integer -> Bool
reversalTest n = reversal(reversal n) == n

testReversal = quickCheck(reversalTest)

{-

The reversal function converts the Integer into a String
(:t read ------ read :: Read a => String -> a),
reverses the String and tries to parse the result as an Integer again.

Cases were this will not work:

- For negative numbers this would result in trying to parse eg: -5 -> 5-
  This will lead to a parse error.

- For numbers ending with a zero this will lead to eg: 1000 -> 0001
  This will result in a failed test as the reversed result is 1.

In our use case the function does work,
because we are only working with primenumbers.

-}
