import Data.List
import Test.QuickCheck
{- time: 1 hour -}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

{- Number of distinctive permutations of a finite set of n elements = n! -}

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

{- The property is hard to test because the results are factorial N which will soon -}
{- be too much for the computer to calculate quickly. -}
permutationsComp :: Positive Int -> Bool
permutationsComp (Positive n) = factorial n == length (permutations [1..n])

compareFunctions :: Positive Int -> Bool
compareFunctions (Positive n) = sort (perms [1..n]) == sort (permutations [1..n])

{- We're testing whether the permutations function from Data.List has the same output as -}
{- our own Perms function when we call it with the same arguments. We are testing a -}
{- specification, and not a mathematical fact. We are using verbose check to
- see which numbers take "forever -}
test :: IO()
{- test = verboseCheck permutationsComp -}
test = verboseCheck compareFunctions
