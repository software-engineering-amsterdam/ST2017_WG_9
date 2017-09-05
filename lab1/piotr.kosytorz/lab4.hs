import Data.List
import Test.QuickCheck

{- The following functions were given in the assigment -}

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversals = takeWhile(<1000) (filter (prime.reversal) primes)

{- Testing: for any element in list, its reversal must be also in the list -}

isInSet :: Integer -> Bool
isInSet = \ n -> (n `elem` reversals) --> reversal n `elem` reversals

test = quickCheckResult(isInSet)
