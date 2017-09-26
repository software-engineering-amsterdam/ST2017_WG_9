module Lab4ex6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lab4ex5

-- Time Spent: 30m

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos l@(x:xs) = subProcess l ++ trClos xs

subProcess :: Ord a => Rel a -> Rel a
subProcess [] = []
subProcess (x:xs) = [x] ++ [x] @@ xs ++ subProcess xs
