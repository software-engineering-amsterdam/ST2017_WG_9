{- time: 0.25 hours -}

isPrime :: Int -> Bool
isPrime n = not (any (\x -> n `mod` x == 0) [2..(n-1)])

predicate :: Int -> Bool
predicate n = n > 0 && isPrime n

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..] 

primeWindow :: Int -> Int
primeWindow n
    | isPrime (sum (take 101 (reverse (take n primes)))) = sum (take 101 (reverse (take n primes)))
    | otherwise = primeWindow (n+1)

runner = primeWindow 101


