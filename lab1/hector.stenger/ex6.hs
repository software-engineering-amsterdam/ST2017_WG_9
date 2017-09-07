{- time: 0.25 hours -}

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..] 

increasingWindow :: Int -> [Int]
increasingWindow n
    | prime (product nPrimes + 1) = increasingWindow (n+1)
    | otherwise = nPrimes
    where nPrimes = take n primes

runner = increasingWindow 2
