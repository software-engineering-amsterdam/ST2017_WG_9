module Lab1 where
import Data.List
import Test.QuickCheck

-- Time : 20 min

assum :: Int -> Int
assum n = length (permutations [1..n])

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

form :: Int -> Int
form n =  fac n

problem :: Positive Int -> Bool
problem (Positive n) = assum n == form n

problem2 :: Positive Int -> Bool
problem2 (Positive n) = fac n == length (permutations [1..n])

test = quickCheck problem

{-
Is the property hard to test?
Yes

If you find that it is, can you given a reason why?
It takes a long time to generate the permutations depending on the size of the created list


Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually?
Are you checking a mathematical fact?
Or are you testing whether perms satisfies a part of its specification?
Or are you testing something else still?
-}
