import Data.List
{- Time: 2 hours. -}

{- My general work of thought is the following. I create a list for every property -}
{- (Rectangular / NoTriangle / Equilateral / Isosceles) that has triples in it -}
{- that only have this property. I then feed my triangle function all of these triples and -}
{- check whether these return values are all of the same type. The only property I don't test -}
{- is 'Other' since that logically is the only one left after checking the previous properties -}

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 
    | isNotPositive x || isNotPositive y || isNotPositive z || isNotTriangle x y z = NoTriangle
    | isEquilateral x y z = Equilateral
    | isIsosceles x y z = Isosceles
    | isRectangular x y z = Rectangular
    | otherwise = Other

isNotTriangle :: Integer -> Integer -> Integer -> Bool
isNotTriangle x y z = not $ lowestTwo > highest
    where sorted = sort [x, y, z]
          lowestTwo = sum $ take 2 sorted
          highest = last sorted 

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z = x == y || x == z || z == y

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z = (x == y && y == z)

isNotPositive :: Integer -> Bool
isNotPositive n = n <= 0

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z
    | x > y && x > z = pythagoramTheorem y z x
    | y > x && y > z = pythagoramTheorem x z y
    | otherwise = pythagoramTheorem x y z

pythagoramTheorem :: Integer -> Integer -> Integer -> Bool
pythagoramTheorem x y z = (x^2 + y^2 == z^2)

checkEquilateral :: Bool
checkEquilateral = all (\r -> triangle r r r == Equilateral) [1..100]

createTriangle :: [Integer] -> Shape
createTriangle (x:y:z:_) = triangle x y z

createSingleNegativeTriple :: Integer -> [Shape]
createSingleNegativeTriple n = map createTriangle (permutations [n, abs n, abs n])

createIsoscelesDoubles :: Integer -> [Shape]
createIsoscelesDoubles n = map createTriangle (permutations [n, n, (n+1)])

checkNoTriangle :: Bool
checkNoTriangle = all (\r -> r == NoTriangle) (concat (map createSingleNegativeTriple [-100..0]))

checkIsosceles :: Bool
checkIsosceles = all (\r -> r == Isosceles) (concat (map createIsoscelesDoubles [2..100]))

{- I got inspired by https://en.wikipedia.org/wiki/Pythagorean_triple for creating a function that -}
{- calculates the first x pythagorean triples. I use these triples to feed my triangle function -}
{- and check if the return value is a Rectangle. -}
genPythagoreanTriples :: [(Integer, Integer, Integer)]
genPythagoreanTriples = [((m^2-n^2),(2*m*n),(m^2+n^2)) | n <- [1..100], m <- [(n+1)..100]]

checkRectangle :: Bool
checkRectangle = all (\r -> r == Rectangular) (map (\(x,y,z) -> triangle x y z) genPythagoreanTriples)
