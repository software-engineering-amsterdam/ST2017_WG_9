module Exercise1 where

import Lecture6
import Test.QuickCheck

{------------------------------------------------------------------------------
Time spent: 1h 30m, Try algorithm on paper, translate to haskell, test and output


x ^ 33 `mod` 5 = 4
x ^ 32 `mod` 5 = 1

x ^ 33 `mod` 5 = (x ^ 32 `mod` 5) * x `mod` 5
x ^ 2 `mod` 5 = x `mod` 5 * x ^ `mod` 5


Example of calculation output:
*Exercise1a> exMout 33 5 7
(1 * 1) `mod` 7 = 1
((33 * 1) `mod` 7) = 5
(5 * 5) `mod` 7 = 4
(4 * 4) `mod` 7 = 2
((33 * 2) `mod` 7) = 3
3

To understand performance improvements run with a large y
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

Compared to the very very very large value of x^y we only handle small numbers
inside this efficient algorithm.


TEST:
Even though a test was not part of the assignment I wanted to write a test to be
sure that the implementation is correct by comparing it against the Lecture and
the Haskell implementation (`mod`)

*Exercise1a> quickCheck testExM'
+++ OK, passed 100 tests.

------------------------------------------------------------------------------}

-- This algorithm was included as exM inside Lecture6
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' x y n
  | even y = w
  | otherwise = (x * w) `mod` n
  where
    z = exM' x (y `div` 2) n
    w = (z * z) `mod` n


-- Extra:

exMout :: Integer -> Integer -> Integer -> IO Integer
exMout _ 0 _ = return 1
exMout x y n
  | even y = do
    z <- exMout x (y `div` 2) n
    let r = ((z * z) `mod` n)
    putStrLn ("("++ show z ++ " * " ++ show z ++ ") `mod` " ++ show n ++ " = " ++ show r)
    return r
  | otherwise = do
    z <- exMout x (y `div` 2) n
    let w = (z * z) `mod` n
    let r = ((x * w) `mod` n)
    putStrLn ("("++ show z ++ " * " ++ show z ++ ") `mod` " ++ show n ++ " = " ++ show w)
    putStrLn ("(("++ show x ++ " * " ++ show w ++ ") `mod` " ++ show n ++ ") = " ++ show r)
    return ((x * w) `mod` n)


testExM' :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
testExM' (Positive x) (Positive y) (Positive m) = exM' x y m == x ^ y `mod` m && exM' x y m == expM x y m
