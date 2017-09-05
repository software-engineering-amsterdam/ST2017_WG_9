module Lab1 where
import Data.List
import Test.QuickCheck

 -- Time 2h - gave up

sieve :: [Integer] -> [Integer]
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)
eprimes = sieve [2..]

-- https://stackoverflow.com/questions/8529814/get-a-sublist-in-haskell
slice :: Int -> Int -> [Integer] -> [Integer]
slice b e l = take (e - b) ( drop b l)

isSum101Prime :: [Integer] -> Bool
isSum101Prime n = sum n `elem` eprimes

calc101Sums :: Int -> Bool
calc101Sums n = isSum101Prime(s)
  where
    s =  slice n (n+100) eprimes

get101s :: [Bool]
get101s = map calc101Sums [0..]

{-
bla :: Integer
bla = sum(slice i (i+100) eprimes)
 where
   i = fromJust head(elemIndex True get101s)
-}

-- redo 20 min

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

getPrimes101 :: [Integer] -> Integer
getPrimes101 l
  | prime(sum(take 101 l)) = sum(take 101 l)
  | otherwise = getPrimes101(tail l)
