module Lab1 where
import Data.List
import Test.QuickCheck

 -- Time 1h

reversal :: Integer -> Integer
reversal = read . reverse . show

sieve :: [Integer] -> [Integer]
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)
eprimes = sieve [2..]

isReversalPrime :: Integer -> Bool
isReversalPrime n = reversal(n) `elem` eprimes

reversalPrimes :: [Integer]
reversalPrimes = filter isReversalPrime eprimes
