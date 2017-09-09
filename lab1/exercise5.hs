module Exercise5 (test) where

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

solution :: [Integer]
solution = findVectorInHaystack primes

sumOfSolution :: Integer
sumOfSolution = sum solution
{- Result is always: 37447 -}

{-
Do you have to test that your answer is correct?
How could this be checked?

> This is a simple pro-forma test. It doesn't make sense to test this funtion,
as it's implementation would be the same as the method itself. We have to assume
that all the functions that were used are implemented correctly.

We are therefore only checking if the resulting number is still prime.
-}
test :: Bool
test = prime sumOfSolution
