{-
Time: 1h (Implementation of classifier was simple, generating test cases for
every possible triangle classication was harder)

To test the classifier I have developed generator functions that generate all
the triangles of a certain shape up to a maximum length of n.

The actual test functions are:
testNoTriangles
testEquilateralTriangles
testIsoscelesTriangles
testRectangularTriangles

Each of them tests triangles up to a length of 50 per side. If you try them
out you will see that each test return True.

Thanks to the testTriangles function it was easy to reduce code duplication
inside the test cases.

-}

module Exercise2 where

import Data.List

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = triangleSorted(sort [x,y,z])

{- Define triangleTriple that takes the parameters in a diffrent form for
convinience (used in the tests) -}
triangleTriple :: (Integer,Integer,Integer) -> Shape
triangleTriple (x,y,z) = triangle x y z

triangleSorted :: [Integer] -> Shape
triangleSorted [x,y,z]
  | x + y <= z = NoTriangle
  | x == y && y == z = Equilateral
  | x == y || x == z || y == z = Isosceles
  | x ^ 2 + y ^ 2 == z ^ 2 = Rectangular
  | otherwise = Other
triangleSorted _ = error "Not enough sides provided!"

{-
Generate all invalid triangles from 0 to n

Example: generateNoTriangle 5
[(0,0,0),(1,0,0),(2,0,0),(2,0,1),(2,1,0),(2,1,1),(3,0,0),(3,0,1),(3,1,0),(3,1,1)
,(4,0,0),(4,0,1),(4,0,2),(4,1,0),(4,1,1),(4,1,2),(4,2,0),(4,2,1),(4,2,2),(5,0,0)
,(5,0,1),(5,0,2),(5,1,0),(5,1,1),(5,1,2),(5,2,0),(5,2,1),(5,2,2)]
-}
generateNoTriangles :: Integer -> [(Integer, Integer, Integer)]
generateNoTriangles n = [(x,y,z) | x <- [0..n] , y <- [0..(x `div` 2)] , z <- [0..(x `div` 2)]]

{-
Example:
generateEquilateralTriangle 10
[(1,1,1),(2,2,2),(3,3,3),(4,4,4),(5,5,5),(6,6,6),(7,7,7),(8,8,8),(9,9,9)
,(10,10,10)]
-}
generateEquilateralTriangles :: Integer -> [(Integer, Integer, Integer)]
generateEquilateralTriangles n = [(x,x,x) | x <- [1..n]]

{-
Example:
generateIsoscelesTriangles 5
[(1,2,2),(1,3,3),(1,4,4),(1,5,5),(2,3,3),(2,4,4),(2,5,5),(3,2,2),(3,4,4),
(3,5,5),(4,3,3),(4,5,5),(5,3,3),(5,4,4)]
-}
generateIsoscelesTriangles :: Integer -> [(Integer, Integer, Integer)]
generateIsoscelesTriangles n = [(x,y,y) | x <- [1..n] , y <- [1..n], x /= y && 2 * y > x]

{-
Example: generateRectangularTriangles 5
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}
generateRectangularTriangles :: Integer -> [(Integer, Integer, Integer)]
generateRectangularTriangles n = [(a,b,c) | a<-[1..n], b<-[1..n], c<-[1..n], (a^2)+(b^2) == (c^2)]

{-
testTriangles(lengthBoundaryOfTriangles, trianglesGeneratorFunction, expectedShape)
Helper function to test classication against generated triangles
-}
testTriangles :: Integer -> (Integer -> [(Integer, Integer, Integer)]) -> Shape -> Bool
testTriangles n g s = all (\x -> triangleTriple x == s) (g n)

testNoTriangles :: Bool
testNoTriangles = testTriangles 50 generateNoTriangles NoTriangle

testEquilateralTriangles :: Bool
testEquilateralTriangles = testTriangles 50 generateEquilateralTriangles Equilateral

testIsoscelesTriangles :: Bool
testIsoscelesTriangles = testTriangles 50 generateIsoscelesTriangles Isosceles

testRectangularTriangles :: Bool
testRectangularTriangles = testTriangles 50 generateRectangularTriangles Rectangular
