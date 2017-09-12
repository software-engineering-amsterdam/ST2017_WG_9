module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Time Spent:
-- ~ 1h on implementation

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

property1 :: Int -> Bool
property1 = (\ x -> even x && x > 3)

property2 :: Int -> Bool
property2 = even

property3 :: Int -> Bool
property3 = (\ x -> even x || x > 3)

property4 :: Int -> Bool
property4 = (\ x -> (even x && x > 3) || even x)

{-
properties = [property1,property2,property3,property4]

strengthList :: [Int -> Bool] -> [Int -> Bool]
strengthList [] = []
strengthList (x:y:xs)
  | stronger [-10..10] x y = x:strengthList(y:xs)
  | otherwise = y:strengthList(x:xs)
-}
--
{-

Lab2> stronger [-10..10] property1 property2
True
*Lab2> stronger [-10..10] property1 property3
True
*Lab2> stronger [-10..10] property1 property4
True
*Lab2> stronger [-10..10] property2 property3
True
*Lab2> stronger [-10..10] property2 property4
True
*Lab2> stronger [-10..10] property3 property4
False

->

1. property1
2. property2
3. property4
4. property3

-}
