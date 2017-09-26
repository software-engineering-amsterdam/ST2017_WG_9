module Lab4ex5 where

import Data.List
import System.Random
import Test.QuickCheck

-- Time Spent: 30m

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos l = remDups (symClosHelper l)

symClosHelper :: Ord a => Rel a -> Rel a
symClosHelper [] = []
symClosHelper ((a,b):xs) = (a,b):(b,a):symClosHelper xs

remDups :: Ord a => Rel a -> Rel a
remDups [] = []
remDups (x:xs)
  | elem x xs = remDups xs
  | otherwise = x:remDups xs

testRel :: Rel Int
testRel = [(1,2),(2,3),(3,4)]

testRel2 :: Rel Int
testRel2 = [(1,2),(2,1),(2,3),(3,4)]
