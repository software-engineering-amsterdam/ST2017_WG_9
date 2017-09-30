module Exercise7 where

-- | Exercise7
-- | ===========================================================================
-- | Time spent: 4h

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import Exercise5
import Exercise6

-- | random relation generator
-- | ===========================================================================

randomOneElemRel :: IO (Rel Int)
randomOneElemRel = do
  x <- randomRIO (1,15)
  y <- randomRIO (1,15)
  return [(x, y)]

randomFixedSizeRel :: Int -> IO (Rel Int)
randomFixedSizeRel 0 = do
  r <- randomOneElemRel
  return r
randomFixedSizeRel n = do
  r <- randomOneElemRel
  re <- randomFixedSizeRel (n-1)
  return $ unionRel r re

randomSizeRel :: IO (Rel Int)
randomSizeRel = do
  s <- randomRIO (0,100)  -- random relation size
  rel <- randomFixedSizeRel s
  return $ nub rel

-- | symClos test
-- | ===========================================================================
-- | Symmetric closure of a binary relation R on a set X is the smallest
-- | symmetric relation on X that contains R. (https://en.wikipedia.org/wiki/Symmetric_closure)
-- | In other words, the symmetric closure of R is the union of R with its inverse relation, R^(-1).

-- | Testing technique:
-- | - check if every element has its symetric equivalent

prop_own_gen_symClos :: IO (Rel Int) -> IO (Bool)
prop_own_gen_symClos ioRel = do
  rel <- ioRel
  let clos = symClos rel
  return (
    all (\x -> (snd x, fst x) `elem` clos) clos
    )

test_own_gen_symClos :: IO (Bool)
test_own_gen_symClos = prop_own_gen_symClos randomSizeRel

-- | trClos test
-- | ===========================================================================
-- | Transitive closure of a binary relation R on a set X is the smallest
-- | relation on X that contains R and is transitive.
-- | https://en.wikipedia.org/wiki/Transitive_closure

-- | Testing technique:
-- | - check if the closure contains R
-- | - check if it is transitive

relationPairs :: Eq t => [t] -> [(t, t)]
relationPairs r = [(x,y) | x <- r, y <- r, x /= y]

-- | Rel is transitive if for any pair of two elements in this relation
-- | (a,b) and (c,d) exist a relation (b,c).
-- | I'm skipping relation on itself (x,x) -- this is important! - check example 4.20 in "Logic in action"
isTransitive :: Eq a => Rel a -> Bool
isTransitive r = all (
  \pair -> (snd(fst pair), (fst(snd pair))) `elem` r
  ) (filter (\x -> snd(fst x) /= (fst(snd x)) ) $ relationPairs r) -- filtering out transitive pairs

prop_own_gen_trClos :: IO (Rel Int) -> IO (Bool)
prop_own_gen_trClos ioRel = do
  rel <- ioRel
  let clos = trClos rel
  return (
    isSubRel rel clos &&
    isTransitive clos
    )

test_own_gen_trClos :: IO (Bool)
test_own_gen_trClos = prop_own_gen_trClos randomSizeRel

-- | QuickCheck
-- | ===========================================================================

-- | trClos test
-- | ===========================================================================
-- | a property for testing symetric closure
prop_symClos :: [(Positive Int,Positive Int)] -> Bool
prop_symClos r = all (\x -> (snd x, fst x) `elem` (symClos r)) (symClos r)

-- | testing with verboseCheck (yields tested values)
test_symClos = quickCheck prop_symClos

-- | trClos test
-- | ===========================================================================
-- | a property for testing transitive closure
prop_trClos :: [(Positive Int,Positive Int)] -> Bool
prop_trClos r = isSubRel r (trClos r) && isTransitive (trClos r)

-- | testing with verboseCheck (yields tested values)
test_trClos = quickCheck prop_trClos

{-- TODO: Fix isTransitive function (it breaks down) --}
{-- TODO: Write test report --}
