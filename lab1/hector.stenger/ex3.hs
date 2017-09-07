import Data.List
import Test.QuickCheck
{- time: 0.5 hours -}

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- function for checking whether an Int is a natural number -}
isNatural :: Int -> Bool
isNatural n = n >= 0

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

lhs :: Int -> Int
lhs n = factorial n
{- lhs = n! -}

rhs :: Int -> Int
rhs n = length (permutations [1..n])
{- rhs = | perm [1..n] | -}

runner = quickCheck(\n -> isNatural n --> (lhs n == rhs n))
