{--
Time spent: 30m
--}

module Exercise4 where

{--
Exercise 4:

The natural number 13 has the property that it is prime and its reversal,
the number 31, is also prime. Write a function that finds all primes < 10000
with this property.

How would you test this function, by the way?
//TODO
--}

{-- reversal function taken from exercise description --}
reversal :: Integer -> Integer
reversal = read . reverse . show

{--function to find primes with Eratosthenes’ sieve taken from lecture --}
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

eprimes :: Integer -> [Integer]
eprimes n = sieve [2..n]

containsWithReverse :: Integer -> [Integer] -> Bool
containsWithReverse x l
  | x < 10 = False {--Do not accept simple digits as a reversal for themselves --}
  | otherwise = x `elem` l && reversal x `elem` l

reversablePrimes :: Integer -> [Integer]
reversablePrimes x = filter (`containsWithReverse` l) l
  where l = eprimes x
