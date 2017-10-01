module Exercise8 where

import System.Random
import Test.QuickCheck
import SetOrd
import Exercise5
import Exercise6
import Exercise7

{- Time spend: 1.25 hours -}

removeItem (x:xs) y
    | x == y = xs
    | otherwise = x : removeItem xs y

{- Sorts relations -}
relSort :: Ord a => Rel a -> Rel a
relSort [x] = [x]
relSort r@(x:xs) = min : relSort remain
    where min = minimum r
          remain = removeItem r min

{- trClos and symClos are order dependent. As you can see in the example below. -}
{- in the simplest case the trClos only adds 1 extra pair, while symClos will add two -}
{- pairs. This is already the simplest example that will go wrong. -}
testRunner = do
    rel <- getRandomSet
    return $ (relSort $ trClos $ symClos rel) == (relSort $ symClos $ trClos rel)
