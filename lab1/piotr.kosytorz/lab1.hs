import Data.List
import Test.QuickCheck

{- Excersise 2 Redo -}

f1, f2 :: Positive Int -> Int
f1 (Positive n) = sum (map (\ n -> n^2) [1..n])
f2 (Positive n) = (n*(n+1)*(2*n+1)) `div` 6

{- natural numbers are >= 0 -}
test1 = quickCheckResult (\n -> f1 n == f2 n)

{- Excersise 3 Redo -}

f3, f4 :: Positive Int -> Int
f3 (Positive n) = sum (map (\ n -> n^3) [1..n])
f4 (Positive n) = ((n*(n+1) `div` 2)^2) `div` 6

{- natural numbers are >= 0 -}
test2 = quickCheckResult (\n -> f4 n == f4 n)
