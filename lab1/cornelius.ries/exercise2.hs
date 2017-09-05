module Lab1 where
import Data.List
import Test.QuickCheck

-- Time: 20 min

assum :: [Int] -> Int
assum n = length(subsequences(n))

form :: [Int] -> Int
form n = 2 ^ length(n)

problem :: [Int] -> Bool
problem n = assum n == form n

test = quickCheck problem

{-

Is the property hard to test?
Yes

If you find that it is, can you given a reason why?
Depending on the size of the list QuickCheck creates, it will take a long time to create the subsequences

Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually?
Are you checking a mathematical fact?
Or are you testing whether subsequences satisfies a part of its specification?
Or are you testing something else still?

-}
