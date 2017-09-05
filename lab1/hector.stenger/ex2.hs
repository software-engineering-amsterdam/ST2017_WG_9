import Data.List
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- function for checking whether an Int is a natural number -}
isNatural :: Int -> Bool
isNatural n = n >= 0

lhs :: Int -> Int
lhs n = length(subsequences [1..n])

rhs :: Int -> Int
rhs n = 2^n

runner = quickCheck(\n -> isNatural n --> (lhs n == rhs n))


