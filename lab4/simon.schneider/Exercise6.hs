module Exercise6 where

import Lecture4(fix,iterateFix)
import Data.List

{--
Task
transitive closure of a relation
trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

Exercise6> trClos relation1
[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

--}

type Rel a = [(a,a)]

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

relation1 :: Rel Int
relation1 = [(1,2),(2,3),(3,4)]

trClos :: Ord a => Rel a -> Rel a
trClos r = (r,2) $$ fix (\ f (r1,n) ->
  if compositeUnion r1 (n+1) == compositeUnion r1 n
    then
      compositeUnion r1 n
    else
      f (r,n+1))


toRelation :: Ord a => [(a,a)] -> Rel a
toRelation xs = sort ( nub xs)

-- TODO: What is R.R called? (not self composite)
compositeUnion :: Ord a => Rel a -> Int -> Rel a
compositeUnion r n = foldl (\ri _ -> toRelation(ri ++ (ri @@ r))) r [1..(n-1)]
