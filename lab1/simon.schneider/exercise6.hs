module Exercise6 where

import Data.List


primes :: [Integer]
primes = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y ^ 2 <= n) primes

fitsRule :: Int -> Bool
fitsRule i = prime( product (take i primes) + 1)

findCounterExample :: Maybe Int
findCounterExample = find ( not . fitsRule ) [1..]

{--Output counter example in a nice sentence:
Found counter example *[2,3,5,7,11,13] + 1 = 30031
--}
outputCounterExample :: Maybe Int -> IO()
outputCounterExample (Just n) = putStrLn ("Found counter example: product of " ++
                                show (take n primes) ++ " + 1 = " ++
                                show (product(take n primes) + 1))
outputCounterExample _ = putStrLn "No counter example found"
