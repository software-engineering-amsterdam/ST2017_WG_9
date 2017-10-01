module Exercis5 where

import SetOrd
import Exercise3

{- Time spend: 1 hour -}

type Rel a = [(a,a)]

invRel :: Rel a -> Rel a
invRel [x] = [(snd x, fst x)]
invRel (x:xs) = invRel [x] ++ invRel xs

{- According to wikipedia 'the symmetric closure of R -}
{- is the union of R with its inverse relation'. -}
{- https://en.wikipedia.org/wiki/Symmetric_closure -}
{- The only edge case is symClos can't contain duplicates. -}
{- we therefor need to filter the inverse relation -}
symClos :: Ord a => Rel a -> Rel a
symClos x = x ++ inverse
    where inverse = filter (\n -> not $ elem n x) $ invRel x

