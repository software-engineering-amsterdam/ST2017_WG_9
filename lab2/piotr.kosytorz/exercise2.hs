import Data.List

{-- Time spent: 45m: 10m for the problem, 35m for the tests --}

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | a + b <= c || a + c <= b || b + c <= a = NoTriangle -- it's a weak precondition, a stronger would be: a > b && a > c && a >= b+c || b > a && b > c && b >= a+c || c > b && c > a && c >= a+b
  | a == b && b == c = Equilateral
  | (a == b && a /= c) || (a == c && a /= b) || (b == c && b /= a) = Isosceles
  | (a*a + b*b == c*c) || (b*b + c*c == a*a) || (c*c + a*a == b*b) = Rectangular
  | otherwise = Other

triangleToupleWrapper :: (Integer, Integer, Integer) -> Shape
triangleToupleWrapper (a,b,c) = triangle a b c

{--
  Correctness of the programme can be checked only with some previously prepared data, that we know that
  should produce a certain output. The program itself is built upon definition of each type a triangle.

  An important note however is one that the data set generator must have stronger preconditions than the
  triangle function itself to produce correct data.
--}

-- help test function
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a > b && a > c && a < b+c || b > a && b > c && b < a+c || c > b && c > a && c < a+b

-- Test sets:
noTriangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a > b && a > c && a >= b+c || b > a && b > c && b >= a+c || c > b && c > a && c >= a+b] -- preconditions for generator is stronger than for the test
equilaterals = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a == b && b == c ] -- preconditions of the generator are of the same strength as of the test
isosceleses = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], isTriangle a b c && ((a == b && a /= c) || (a == c && a /= b) || (b == c && b /= a)) ] -- preconditions of the generator must be stronger that the ones of the test
rectangulars = [ (a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] -- this precondition is sufficient for both: generator and test

testNoTriangles = or $ map(\x -> x == Rectangular) $ map triangleToupleWrapper noTriangles
testEquilaterals = or $ map(\x -> x == Rectangular) $ map triangleToupleWrapper equilaterals
testIsosceleses = or $ map(\x -> x == Rectangular) $ map triangleToupleWrapper isosceleses
testRectangulars = or $ map(\x -> x == Rectangular) $ map triangleToupleWrapper rectangulars
-- no testing others
