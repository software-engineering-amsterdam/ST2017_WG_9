
module Exercise4 where

import Test.QuickCheck

reversal :: Integer -> Integer
reversal = read . reverse . show

isPrime :: Integer -> Bool
isPrime n = not (any (\x -> n `mod` x == 0) [2..(n-1)])

predicate :: Integer -> Bool
predicate n = n > 0 && isPrime n && isPrime (reversal n)

findPrimesIn1000 :: [Integer]
findPrimesIn1000 = filter predicate [1..10000]

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
