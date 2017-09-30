module Lab4ex7 where

import Data.List
import System.Random
import Test.QuickCheck
import Lab4ex5
import Lab4ex6

-- Time Spent: 15m

{-

QuickCheck Generator
after writing i got error messages for already existing arbitrary
so is commented it out and tried a simple property down below with QuickCheck


type RelInt = Rel Int

instance Arbitrary RelInt where
    arbitrary = genRel

genRel :: Gen RelInt
genRel =
      do
        size <- elements [1..100]
        genRelS size []

genRelS :: Int -> RelInt -> Gen RelInt
genRelS n s =
  if n > 0
    then do
      a <- elements [1..100]
      b <- elements [1..100]
      genRelS (n-1) ((a,b):s)
  else return s
-}

-- The length of symClos and trClos should be equal or larger than original R
testLengthSymClos :: Rel (Positive Int) -> Bool
testLengthSymClos l = length l <= length (symClos l)

testLengthTrClos :: Rel (Positive Int) -> Bool
testLengthTrClos l = length l <= length (trClos l)
