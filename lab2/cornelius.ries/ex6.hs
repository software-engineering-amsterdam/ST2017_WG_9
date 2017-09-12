module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Maybe

-- Time Spent:
--

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

{-

ROT 13 turns every character (a..z,A..Z) of a String to the +13th element
in the alphabet of the original char. All other characters stay the same.

- Applying the function twice yields the original input.
- The function will not change the length of the input
- T

Domain: UPPER Case Characters From A..Z

-}

upperChars = ['A'..'Z']
lowerChars = ['a'..'z']

rot13 :: String -> String
rot13 [] = ""
rot13 (x:xs)
  | x `elem` upperChars = (upperChars !! newIndex):[] ++ rot13 xs
  | x `elem` lowerChars = (lowerChars !! newIndex):[] ++ rot13 xs
  | otherwise = x:[] ++ rot13 xs
  where
    index = fromJust (findIndex (==toLower(x)) lowerChars)
    newIndex = (index + 13) `mod` 26

-- test the Reversal
testReversal :: String -> Bool
testReversal s = rot13(rot13 s) == s
-- test the Length
testLength :: String -> Bool
testLength s = length(rot13 s) == length s
