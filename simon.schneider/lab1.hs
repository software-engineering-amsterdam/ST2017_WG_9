module Lab1 where
import Data.List
import Test.QuickCheck

t2_sum :: Int -> Int
t2_sum n
  | n == 0 = 0
  | n < 0 = error "Negative numbers are not allowed."
  | otherwise = n ^ 2 + t2_sum (n - 1)

t2_formula :: Int -> Int
t2_formula n
  | n < 0 = error "Negative numbers are not allowed."
  | otherwise = (n * (n + 1) *) (2 * n +1) `div` 6

t2 :: Positive Int -> Bool
t2 (Positive n) = t2_sum n == t2_formula n

t3_sum :: Int -> Int
t3_sum n
  | n == 0 = 0
  | n < 0 = error "Negative numbers are not allowed."
  | otherwise = n ^ 3 + t3_sum (n - 1)

t3_formula :: Int -> Int
t3_formula n
  | n < 0 = error "Negative numbers are not allowed."
  | otherwise = ((n * (n+1)) `div` 2 ) ^ 2

t3 :: Positive Int -> Bool
t3 (Positive n) = t3_sum n == t3_formula n

t4_subsequenceSize1 :: Int -> Int
t4_subsequenceSize1 0 = 1
t4_subsequenceSize1 n = length ( subsequences [1..n]  )

t4_subsequenceSize2 :: Int -> Int
t4_subsequenceSize2 n = 2 ^ n

t4 :: Positive Int -> Bool
t4 (Positive n) = t4_subsequenceSize1 n == t4_subsequenceSize2 n
