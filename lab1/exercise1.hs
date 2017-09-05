module Exercise1 (t2, t3) where
import Test.QuickCheck

{--
TODO: Brag about usage of positive type
TODO: Show piotrs approach as an alternative (why not used?)
--}

t2_sum :: Int -> Int
t2_sum n
  | n == 0 = 0
  | otherwise = n ^ 2 + t2_sum (n - 1)

t2_formula :: Int -> Int
t2_formula n = (n * (n + 1) *) (2 * n +1) `div` 6

t2 :: Positive Int -> Bool
t2 (Positive n) = t2_sum n == t2_formula n

t3_sum :: Int -> Int
t3_sum n
  | n == 0 = 0
  | otherwise = n ^ 3 + t3_sum (n - 1)

t3_formula :: Int -> Int
t3_formula n = ((n * (n+1)) `div` 2 ) ^ 2

t3 :: Positive Int -> Bool
t3 (Positive n) = t3_sum n == t3_formula n

{-- Tests: --}
test_t2 :: IO()
test_t2 = verboseCheck t2

test_t3 :: IO()
test_t3 = verboseCheck t3
