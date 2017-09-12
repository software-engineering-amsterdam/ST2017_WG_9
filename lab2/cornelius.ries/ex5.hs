module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Time Spent:
--

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

checkDerangement :: Eq a => [a] -> [a] -> Bool
checkDerangement [] [] = True
checkDerangement _ [] = False
checkDerangement [] _ = False
checkDerangement (x:xs) (y:ys) = x /= y && checkDerangement xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement _ [] = False
isDerangement [] _ = False
isDerangement l1 l2 = length l1 == length l2 &&
    all (\x -> x `elem` l2) l1 &&
    checkDerangement l1 l2

deran :: Eq a => [a] -> [[a]]
deran l = filter (\x -> isDerangement x l) (permutations l)

{-
testLength :: Positive Int -> Bool
testLength (Positive n) = not (isDerangement [1..n] [1..n])

testLength2 :: [Int] -> [Int] -> Bool
testLength2 l1 l2 = isDerangement l1 l2 --> length l1 == length l2

testElements :: Int -> Bool
testElements n = isDerangement [-n..(-1)] [1..n]
-}

testSame :: Int -> Bool
testSame n = isDerangement l1 l2 == isDerangement l2 l1
  where l1 = [1..n]
        l2 = [1..n]

testReverse :: Int -> Bool
testReverse n = isDerangement l1 l1 == False
  where l1 = [1..n]
