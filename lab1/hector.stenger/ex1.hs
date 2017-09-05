import Test.QuickCheck
{- time: 2.5 hours, was working on using Gen for quickcheck -}
{- for an hour until I figured out that implication was a better -}
{- alternative. -}

{- implication infix operator -}
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- function for checking whether an Int is a natural number -}
isNatural :: Int -> Bool
isNatural n = n >= 0

{- left-hand side of exercise 2 -}
{- using recursion with a base case of 0 to calculate the value -}
lhs2 :: Int -> Int
lhs2 0 = 0
lhs2 n = n ^ 2 + lhs2 (n-1)

{- right-hand side of exercise 2 -}
rhs2 :: Int -> Int
rhs2 n = (n * (n+1) * ((2 * n) + 1)) `div` 6

{- runner for the second exercise, check whether the output for natural numbers -
- on the lhs is equal to the rhs. We are using the implication operator to garantee the fact -
- that the evaluation only depends on natural numbers. Whenever quickCheck
- uses an negative number the nature of the implicaton operator is used to
- always return a true value-}
runner2 = quickCheck(\n -> isNatural n --> (lhs2 n == rhs2 n))

{- left-hand side of exercise 3 -}
{- using recursion with a base case of 0 to calculate the value -}
lhs3 :: Int -> Int
lhs3 0 = 0
lhs3 n = n^3 + lhs3 (n-1)

{- right-hand side of exercise 3 -}
rhs3 :: Int -> Int
rhs3 n = ((n * (n + 1)) `div` 2)^2

runner3 = quickCheck(\n -> isNatural n --> (lhs3 n == rhs3 n))
