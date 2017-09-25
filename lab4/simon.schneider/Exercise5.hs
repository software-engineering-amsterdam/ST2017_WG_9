module Exercise5 where

import SetOrd
import Data.List

type Rel a = [(a,a)]

{--
Time spent: 30m

that gives the symmetric closure of a relation, where the relation is
represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)]
should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].

From book (Haskell way to logic p 171):
R UNION R^(-1) is the symmetric closure of R.
--}



{------------------------------------------------------------------------------
                     Implementation
-------------------------------------------------------------------------------}

symClos :: Ord a => Rel a -> Rel a
symClos r = sort (nub (r ++ relationInverse r))

relationInverse :: Ord a => Rel a -> Rel a
relationInverse xs = map pairInverse xs

pairInverse :: Ord a => (a,a) -> (a,a)
pairInverse (a,b) = (b,a)

{------------------------------------------------------------------------------
                     Test
-------------------------------------------------------------------------------}

relation1 :: Rel Int
relation1 = [(1,2),(2,3),(3,4)]

relation1Closure :: Rel Int
relation1Closure = [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]

test :: Bool
test = symClos relation1 == relation1Closure
