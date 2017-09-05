module Lab1 where
import Data.List
import Test.QuickCheck

-- Time: 3h getting to know haskell / 1h to solve problem

-- the solution for QuickCheck passing only positive numbers was found here
-- http://tutel.me/c/programming/questions/33063730/compare+two+functions+using+quickcheck+to+generate+positive+integers
-- Docs: http://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck-Modifiers.html

assum_2 :: Int -> Int
assum_2 0 = 0
assum_2 n = n ^ 2 + assum_2(n-1)

formula_2 :: Int -> Int
formula_2 n = (n*(n+1)*(2*n+1)) `div` 6

problem_2 :: Positive Int -> Bool
problem_2 (Positive n) = assum_2 n == formula_2 n


assum_3 :: Int -> Int
assum_3 0 = 0
assum_3 n = n ^ 3 + assum_3(n-1)

formula_3 :: Int -> Int
formula_3 n = (n*(n+1)`div`2)^2

problem_3 :: Positive Int -> Bool
problem_3 (Positive n) = assum_3 n == formula_3 n
