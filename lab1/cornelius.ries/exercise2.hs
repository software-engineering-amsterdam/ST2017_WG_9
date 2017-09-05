module Lab1 where
import Data.List
import Test.QuickCheck

-- Time: 20 min

assum_4 :: [Int] -> Int
assum_4 n = length(subsequences(n))

form_4 :: [Int] -> Int
form_4 n = 2 ^ length(n)

problem_4 :: [Int] -> Bool
problem_4 n = assum_4 n == form_4 n


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
