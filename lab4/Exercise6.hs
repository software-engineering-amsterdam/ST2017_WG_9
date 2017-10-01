module Exercise6 where

-- | Exercise6
-- | ===========================================================================
-- | Time spent: 1h

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4

type Rel a = [(a,a)]

-- | Composition of relations: R ∘ S
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- | R ∘ R gives only transitions between current elements of R
-- | The best idea would be to keep applying composition as along as it
-- | doesn't change the relation. For this reason I will use fixpoint
-- | (delared in Lecture4.hs)
trClos :: Ord a => Rel a -> Rel a
trClos r = fp (\x -> (sort.nub) (x ++ (x @@ x))) r

{--

Task
transitive closure of a relation
trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

Exercise6> trClos testRel
[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

--}
testRel :: Rel Int
testRel = [(1,2),(2,3),(3,4)]
