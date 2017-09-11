{--
Time spent: 1h (Getting started with monads, solving the problem, find a test)

If you generate 10000 numbers, then roughly 2500 of them should be in each quartile.
Implement this test, and report on the test results.

> Answer:

getProbsBuckets splits the random numbers into 4 buckets.
The amount of elements in each bucket varies between each execution, but
the values are more or less close to 2500
*Exercise1> getProbsBuckets 10000
(2525,2520,2546,2409)
*Exercise1> getProbsBuckets 10000
(2463,2475,2486,2576)
*Exercise1> getProbsBuckets 10000
(2478,2446,2516,2560)
*Exercise1> getProbsBuckets 10000
(2496,2515,2530,2459)
*Exercise1> getProbsBuckets 10000
(2508,2481,2390,2621)
*Exercise1> getProbsBuckets 10000
(2478,2476,2525,2521)

To see if the amount of elements in each quartile converges to N/4 for large
numbers I have implemented the getProbsTolerance function:
*Exercise1> getProbsTolerance 10
0.2
*Exercise1> getProbsTolerance 10
0.4
*Exercise1> getProbsTolerance 10
0.6
*Exercise1> getProbsTolerance 100
0.18
*Exercise1> getProbsTolerance 100
0.3
*Exercise1> getProbsTolerance 100
0.1
*Exercise1> getProbsTolerance 1000
3.6e-2
*Exercise1> getProbsTolerance 1000
2.6e-2
*Exercise1> getProbsTolerance 1000
3.8e-2
*Exercise1> getProbsTolerance 10000
1.1e-2
*Exercise1> getProbsTolerance 100000
5.18e-3
*Exercise1> getProbsTolerance 1000000
1.6e-3

As we can see the diffrence between an optional randomness generator and the probs
function is reasonable small for 1000000 iterations. Only 1.6e-3 elements belong
to the wrong bucket.

--}

module Exercise1 where

import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

{-
Split floats in buckets like this:
(0..0.25), [0.25..0.5), [0.5..0.75), [0.75..1)
-}
addToBucket :: (Int,Int,Int,Int) -> Float -> (Int, Int, Int, Int)
addToBucket (a,b,c,d) x
    | x <= 0.25 = (a+1, b, c ,d)
    | x > 0.25 && x <= 0.5 = (a, b+1, c, d)
    | x > 0.5 && x <= 0.75 = (a,b, c+1 ,d)
    | otherwise = (a,b,c,d+1)

getBuckets :: (Int,Int,Int,Int) -> [Float] -> (Int,Int,Int,Int)
getBuckets c [] = c
getBuckets c l = foldl addToBucket c l

getProbsBuckets :: Int -> IO (Int,Int,Int,Int)
getProbsBuckets n = do
 l <- probs n
 return (getBuckets (0,0,0,0) l)

getProbsTolerance :: Int -> IO Float
getProbsTolerance n = do
  (a,b,c,d) <- getProbsBuckets n
  return (fromIntegral(abs(a - q) + abs(b - q) + abs(c - q) + abs(d - q)) / fromIntegral n)
  where
    q = n `div` 4
