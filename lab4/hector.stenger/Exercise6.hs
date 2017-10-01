module Exercise6 where

import Exercise5
import SetOrd

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{- Inspired by https://stackoverflow.com/questions/31150370/haskell-remove-duplicates-from-list -}
nub :: Eq a => Rel a -> Rel a
nub [] = []
nub (r:rs) = r : nub (filter (/= r) rs)

relUnion :: Eq a => Rel a -> Rel a-> Rel a
relUnion xs ys = nub $ xs ++ ys

{- Check whether the union of R and the transitive between  -}
{- R an R is transitive. If not keep going. -}
trClos :: Ord a => Rel a -> Rel a
trClos r
    | isTransitive comp = comp
    | otherwise = trClos comp
    where comp = relUnion r $ r @@ r

{- If a Rel is transitive that means that when (x,y) (y,z) have a matching y -}
{- the pair of (x,z) should be found in the Rel. If this is not the case then -}
{- the Rel is not transitive -}
isTransitive :: Eq a => Rel a -> Bool
isTransitive r = empty $ [(x,z) | l@(x,y) <- r, ll@(w,z) <- r, (y == w && (not $ elem (x,z) r))]

empty :: Rel a -> Bool
empty r = length r == 0
