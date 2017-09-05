module Lab1 where
import Data.List
import Test.QuickCheck

-- Time : 20 min

assum :: [Int] -> Int
assum n = length(permutations(n))

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

form :: [Int] -> Int
form n =  fac(length(n))

problem :: [Int] -> Bool
problem n = assum n == form n

test = quickCheck problem

{-
Is the property hard to test?
Yes, i takes a long time

If you find that it is, can you given a reason why?
Depending on the size of the list QuickCheck creates, it will take time to create the permutations


Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually?
Are you checking a mathematical fact?
Or are you testing whether perms satisfies a part of its specification?
Or are you testing something else still?
-}
