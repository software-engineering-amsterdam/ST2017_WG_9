import System.Random
import Test.QuickCheck
import SetOrd

{- Time spend: 3 hours, because generators and docs.-}

randomInt :: Int -> Int -> IO Int
randomInt x1 x2 = randomRIO (x1, x2)

randomArray :: Int -> Int -> Int -> IO [Int]
randomArray n x1 x2
    | n > 1 = do
        num <- randomInt x1 x2
        next <- randomArray (n-1) x1 x2
        return $ num : next
    | otherwise = do
        num <- randomInt x1 x2
        return $ num : []

{- Could, should, have used fold here. Just did not now how -}
{- to unpack the Int's from the IO monad. -}
randomSet :: IO (Set Int)
randomSet = do
    lb <- randomInt (-1000) 0
    ub <- randomInt 1 1000
    elems <- randomInt 1 1000
    array <- randomArray elems lb ub
    return $ list2set array

testSet :: Set Int -> Bool
testSet _ = True

testRunner = quickCheck testSet

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        list <- arbitrary
        return $ list2set list
