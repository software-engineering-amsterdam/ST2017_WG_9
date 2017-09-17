module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Time Spent:
-- 10 min on isPermutation
-- 2h on properties and understand how stronger/weaker is used in this case

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l1 l2 = length l1 == length l2 &&
    all (\x -> x `elem` l2) l1 &&
    all (\x -> x `elem` l1) l2

validPerms :: [([Int],[Int])]
validPerms = [([1,2,3],[1,2,3]),
          ([1,2,3],[2,1,3]),
          ([1,2,3],[3,2,1])]

invalidPerms :: [([Int],[Int])]
invalidPerms = [([1,2,3],[3,2]),
                    ([1,2,3],[3,2,0]),
                    ([1,2,3],[3,2,0,4])]

test = all (\x -> isPermutation (fst x) (snd x)) validPerms
      && all (\x -> not(isPermutation (fst x) (snd x))) invalidPerms

samelength :: ([Int],[Int]) -> Bool
samelength (xs,ys) = length xs == length ys

samelist :: ([Int],[Int]) -> Bool
samelist (xs,ys) = xs == ys

{-

testing the properties for stronger/weaker

*Lab2> stronger validPerms samelength samelist
False
*Lab2> stronger validPerms samelist samelength
True

Strength list:
samelist
samelength

-}
