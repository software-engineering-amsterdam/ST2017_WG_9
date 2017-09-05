
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

{--TODO: Implement a test for reversal (reverse twice and check its the same) --}
{--TODO: Argue why its hard to implement a test --}
