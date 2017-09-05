import Data.List
import Test.QuickCheck

{- operator declaration: implication -}
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- Excersise 2 Redo -}

f1, f2 :: Int -> Int
f1 = \ n -> sum (map (\ n -> n^2) [1..n])
f2 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

{- natural numbers are >= 0 -}
test1 = quickCheckResult (\n -> n >= 0 --> f1 n == f2 n)

{- Excersise 3 Redo -}

f3, f4 :: Int -> Int
f3 = \ n -> sum (map (\ n -> n^3) [1..n])
f4 = \ n -> ((n*(n+1) `div` 2)^2) `div` 6

{- natural numbers are >= 0 -}
test2 = quickCheckResult (\n -> n >= 0 --> f4 n == f4 n)
