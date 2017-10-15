module Exercise2 where

import Exercise1
import Lecture6
import System.Random

{------------------------------------------------------------------------------
Time spent: 30m

Task:
Check that your implementation is more efficient than expM by running a number of
relevant tests and documenting the results.

Tests are below:
We already checked for correct implementation in Exercise1 using QuickCheck
The test results show that `expM` performs better for small numbers but ExM'
really shines with very large numbers and is therefore perfect for
cryptography. It even works for extremly large numbers, the `expM` process will
just die of because it couldnt handle numbers of this extreme scale.
(see result for testExtraLargeExpM below)

*Exercise1a> exMout 8123 33101 70
(1 * 1) `mod` 70 = 1
((8123 * 1) `mod` 70) = 3
(3 * 3) `mod` 70 = 9
(9 * 9) `mod` 70 = 11
(11 * 11) `mod` 70 = 51
(51 * 51) `mod` 70 = 11
(11 * 11) `mod` 70 = 51
(51 * 51) `mod` 70 = 11
(11 * 11) `mod` 70 = 51
((8123 * 51) `mod` 70) = 13
(13 * 13) `mod` 70 = 29
(29 * 29) `mod` 70 = 1
((8123 * 1) `mod` 70) = 3
(3 * 3) `mod` 70 = 9
(9 * 9) `mod` 70 = 11
(11 * 11) `mod` 70 = 51
((8123 * 51) `mod` 70) = 13
(13 * 13) `mod` 70 = 29
((8123 * 29) `mod` 70) = 17
(17 * 17) `mod` 70 = 9
(9 * 9) `mod` 70 = 11
((8123 * 11) `mod` 70) = 33
33

Showed that the `ExM'` algorithm does not need to handle very large numbers by
using modular exponentiation.

------------------------------------------------------------------------------}

benchmarkExM :: Int -> Int -> Int -> Int -> (Integer -> Integer -> Integer -> Integer) -> Integer -> IO String
benchmarkExM 0 _ _ _ _ s = return ("Finished: " ++ show s)
benchmarkExM n xMax yMax mMax f s =
  do
    x <- randomRIO (1,xMax)
    y <- randomRIO (1,yMax)
    m <- randomRIO (1,mMax)
    let r = f (toInteger x) (toInteger y) (toInteger m)
    let s2 = s + r
    -- putStrLn (show n ++ ": " ++ show x ++ show y ++ show m ++ " = " ++ show r)
    (benchmarkExM (n-1) xMax yMax mMax f s2)

benchmarkExpM :: Int -> Int -> Int -> Int -> IO String
benchmarkExpM n xMax yMax mMax = benchmarkExM n xMax yMax mMax expM 0

benchmarkExM' :: Int -> Int -> Int -> Int -> IO String
benchmarkExM' n xMax yMax mMax = benchmarkExM n xMax yMax mMax exM' 0

testSmallExpM = benchmarkExpM 100000 50 10 10 -- (0.45 secs, 422,992,288 bytes)
testSmallExM' = benchmarkExM' 100000 50 10 10 -- (0.62 secs, 492,223,464 bytes)
testMiddleExpM = benchmarkExpM 100000 5000 100 90 -- (0.54 secs, 548,929,096 bytes)
testMiddleExM' = benchmarkExM' 100000 5000 100 90 -- (0.99 secs, 652,435,216 bytes)
testLargeExpM = benchmarkExpM 100000 50000 10000 90 -- (13.03 secs, 3,318,452,512 bytes)
testLargeExM' = benchmarkExM' 100000 50000 10000 90 -- (1.64 secs, 1,082,852,536 bytes)
testExtraLargeExpM = benchmarkExpM 1000 50000 1000000 90 -- 16166 segmentation fault  ghci
testExtraLargeExM' = benchmarkExM' 100000 50000 1000000 90 -- (2.34 secs, 1,523,380,128 bytes)
