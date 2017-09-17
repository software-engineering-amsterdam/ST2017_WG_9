import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = elem x $ permutations y

samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

sameitems :: [Int] -> [Int] -> Bool
sameitems [] [] = True
sameitems [x] ys = elem x ys
sameitems (x:xs) ys = elem x ys && sameitems xs ys

sameListCheck = isPermutation [1,2] [1,2] == False
reversedOrderCheck = isPermutation [2,1] [1,2] == True
exampleCheck = isPermutation [2,2,0] [1,2,3] == False

{- Checking whether a list is a permutation of itself -}
equality n = isPermutation n n

{- Checking whether a different order is a permuation -}
differentOrder n = isPermutation n (sort n)
    && isPermutation (sort n) n
    && isPermutation n (reverse n)
    && isPermutation (reverse n) n

{- Precondition should state that both items are lists. -}
{- Postcondition check should be checking whether both lists -}
{- share the same length and have the same items. -}
