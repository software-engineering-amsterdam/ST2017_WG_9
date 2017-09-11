module Lab1 where
import Data.List
import Test.QuickCheck

-- Time: 20 min

assum :: Int -> Int
assum n = length(subsequences [1..n])

form :: Int -> Int
form n = 2 ^ n

problem :: Positive Int -> Bool
problem (Positive n) = assum n == form n

problem2 :: Positive Int -> Bool
problem2 (Positive n) = 2^n == length (subsequences [1..n])

test = quickCheck problem



{-

Is the property hard to test?
Yes

If you find that it is, can you given a reason why?
Depending on the size of the list that i created with quickCheck, it will take a long time to create the subsequences

Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually?
Are you checking a mathematical fact?
Or are you testing whether subsequences satisfies a part of its specification?
Or are you testing something else still?

We are not checking a mathmatical fact, since quickcheck only tests for a fraction of possible outcomes.
We are testing only a part of subsequences's specification.

-}
