module Exercise7 where

import System.Random
import Test.QuickCheck
import SetOrd
import Exercise5
import Exercise6

{- Time spend: 1 hour -}

randomInt :: Int -> Int -> IO Int
randomInt x1 x2 = randomRIO (x1, x2)

randomSet :: Int -> Int -> Int -> IO (Rel Int)
randomSet n x1 x2
    | n > 1 = do
        num1 <- randomInt x1 x2
        num2 <- randomInt x1 x2
        next <- randomSet (n-1) x1 x2
        return $ (num1, num2) : next
    | otherwise = do
        num1 <- randomInt x1 x2
        num2 <- randomInt x1 x2
        return $ (num1, num2) : []

getRandomSet :: IO (Rel Int)
getRandomSet = do
    elems <- randomInt 1 10
    randomSet <- randomSet elems 1 10
    return $ nub $ randomSet

isSubSet :: Rel Int -> Rel Int -> Bool
isSubSet r s = empty $ filter (\n -> not $ elem n s) r

{- Two checks I could think about. The 'original' has to be  -}
{- a subset of the transitive closure and all the elements in -}
{- the closure have to be checked for transitivity -}
transitivity :: Rel Int -> Bool
transitivity r = isSubSet r trans && isTransitive trans
    where trans = trClos r

{- Two checks I could come up with are check if 'original' is -}
{- a subset of the closure and check whether the inverse relation -}
{- is a subset -}
symmetry :: Rel Int -> Bool
symmetry r = isSubSet r sym && isSubSet inv sym
    where sym = symClos r
          inv = invRel r

checkTransitivity = do
    rel <- getRandomSet
    return $ transitivity rel

checkSymmetry = do
    rel <- getRandomSet
    return $ symmetry rel

{- Using QuickCheck you'd have to define an arbitrary for Rel to start testing. -}
{- You could maybe add more testable properties, couldn't come up with more -}
