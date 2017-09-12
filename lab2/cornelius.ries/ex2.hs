module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Time Spent:
-- ~ 30 min on function
-- ~ 1h on thinking of test


data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | not (a + b > c) || not (a + c > c) || not (c + b > a) = NoTriangle
  | a == b && b == c = Equilateral
  | a == b && b /=c || b == c && c /= a || a==c && c/= b = Isosceles
  | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2+c^2 == a^2 = Rectangular
  | otherwise = Other

invT = [(2,2,0),(0,2,2),(2,0,2)]
equiliterals = [(2,2,2),(2,2,2),(2,2,2)]

isExpectedResult ::  Shape -> (Integer, Integer, Integer) -> Bool
isExpectedResult  s (a,b,c) = triangle a b c == s

testNoTriangles :: Bool
testNoTriangles = all (isExpectedResult NoTriangle) invT

testEquilateral :: Bool
testEquilateral = all (isExpectedResult Equilateral) equiliterals
