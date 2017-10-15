module Exercise6 where

import Lecture6Dirty
import Exercise5 (carmichael)

{------------------------------------------------------------------------------
Time spent: 1h (Research Miller Rabin, implement tests, run, make sense)

TASK:
Use the list from the previous exercise to test the MillerRabin
primality check. What do you find?

TESTS:
*Exercise6> compareTests 1 400
False positives Fermat      : 400 ( 100.0%)
False positives Miller-Rabin: 53 ( 13.25%)

*Exercise6> compareTests 2 1000
False positives Fermat      : 1000 ( 100.0%)
False positives Miller-Rabin: 11 ( 1.0999999999999999%)

*Exercise6> compareTests 3 1000
False positives Fermat      : 1000 ( 100.0%)
False positives Miller-Rabin: 4 ( 0.4%)

*Exercise6> compareTests 4 1000
False positives Fermat      : 1000 ( 100.0%)
False positives Miller-Rabin: 2 ( 0.2%)

*Exercise6> compareTests 5 1000
False positives Fermat      : 1000 ( 100.0%)
False positives Miller-Rabin: 0 ( 0.0%)

*Exercise6> compareTests 10 1000
False positives Fermat      : 1000 ( 100.0%)
False positives Miller-Rabin: 0 ( 0.0%)

CONCLUSION:
The miller-rabin algorithm has a much higher accuarcy for detecting false
positives in carmichael number than the fermat algorithm.
If k is larger than 5 there is nearly no chance to find a false positive in a
reasonable time.

Tests based on Fermat's Little Theorem are less effective than strong
probable prime tests such as the Baillie-PSW primality test and the Miller–Rabin
primality test.

(1)
In contrast to fermats test, the Rabin-Miller algorithm also tests, given a
number n, whether Z_n has a nontrivial root of Unity.
For Fermat's test, if a composite number n is not Carmichael, then the
probability that the test will detect compositeness is at least 1/2.
However, the test will fail all Carmichael numbers.

(2)
For the Rabin-Miller test, every composite number will be detected with
probability at least 1/2. This means that the correctness probability is
independent of the input (there are no "hard" inputs).
This is why rabin-miller is stronger than eulers algorithm.

Sources:
https://cs.stackexchange.com/questions/21462/why-miller-rabin-instead-of-fermat-primality-test
https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
https://en.wikipedia.org/wiki/Carmichael_number
-------------------------------------------------------------------------------}


findFalsePositives' :: (Int -> Integer -> IO Bool) -> Int -> [Integer] -> IO [Integer]
findFalsePositives' _ _ [] = return []
findFalsePositives' f k (c:cs) =
  do
    failed <- f k c
    if failed then
      do
        -- putStrLn ("FOUND FALSE POSITIVE: " ++ show c)
        t <- findFalsePositives' f k cs
        return (c : t)
    else
      findFalsePositives' f k cs

testMillerRabin, testFermat :: Int -> Int -> IO [Integer]
testMillerRabin k n = findFalsePositives' primeMR k (take n carmichael)
testFermat k n = findFalsePositives' primeTestsF k (take n carmichael)

compareTests :: Int -> Int -> IO ()
compareTests k n = do
  fnFe <- testFermat k n
  fnMr <- testMillerRabin k n
  let pFe = fromIntegral(length fnFe) / (fromIntegral n) * 100
  let pMr = fromIntegral(length fnMr) / (fromIntegral n) * 100
  putStrLn ("False positives Fermat      : " ++ show (length fnFe) ++ " ( " ++ show pFe ++ "%)")
  putStrLn ("False positives Miller-Rabin: " ++ show (length fnMr) ++ " ( " ++ show pMr ++ "%)")
