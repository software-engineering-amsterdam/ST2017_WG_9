module Lab1 where

 -- Time 1h
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

counterexamples :: [Integer] -> Int -> Integer --[([Integer],Integer)]
counterexamples l n
  | prime(product(take n primes) + 1) = counterexamples l (n + 1)
  | otherwise = product(take n primes) + 1

counterexamples2 :: [Integer] -> Int -> [([Integer],Integer)]
counterexamples2 l n
  | prime(product(take n primes) + 1) = counterexamples2 l (n + 1)
  | otherwise = [(take n primes , product(take n primes) + 1)]

counterexamples3 :: Int -> ([Integer],Integer)
counterexamples3 n
  | not $ prime(product(take n primes) + 1) = (take n primes, product(take n primes) + 1)
  | otherwise = ([], 0)

generateCounters :: [([Integer],Integer)]
generateCounters = filter (\m -> m /= ([], 0)) (map counterexamples3 [1..])

























-- redo
counterexamples4 :: Integer -> ([Integer], Integer)
counterexamples4 n
  | prime cpp = (cp,cpp)
  | otherwise = ([],0)
  where
    cp = take (fromIntegral n) primes
    cpp = product cp + 1


counterexamples4c :: [([Integer], Integer)]
counterexamples4c = filter (\x -> x /= ([],0)) (map counterexamples4 [1..])
