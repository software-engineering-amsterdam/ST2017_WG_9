module Exercise5 where

-- | Exercise5
-- | ===========================================================================
-- | Time spent: 15min

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4

-- I moved the definition of type Rel to Lecture4.hs

-- | basically this is enouh, however if there is i.e. a relation is on itself,
-- | i.e. (4,4), the method will put it twice on the list
symClosDirty :: Ord a => Rel a -> Rel a
symClosDirty [] = []
symClosDirty (x:xs) = [x] ++ [(snd x,fst x)] ++ symClosDirty xs

-- | to make it clean I apply "nub"
symClos :: Ord a => Rel a -> Rel a
symClos x = nub $ symClosDirty x
