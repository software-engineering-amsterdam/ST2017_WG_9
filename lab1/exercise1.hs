module Exercise1 (compareSumOfSquares, compareSumOfCubes) where
import Test.QuickCheck

{--
This exercise tests the following two assumptions:
compareSumOfSquares: 1^2 + 2^2 .... n^2 = (n(n + 1)(2n + 1)) / 6
compareSumOfCubes: 1^3 + 2^3 .... n^3 = (n(n + 1)) / 2) ^ 2

We have decided to use the Positive Int type to represent numbers.
This way we can make sure that only natural numbers are used inside the
`test_compareSumOfSquares` and `test_compareSumOfCubes` function.

Another approach would be to simply ignore the QuickCheck tests that provide
negative numbers by using the implication arrow.
(Please see ./piotr.kosytorz/lab1.hs for this differen solution)

This would skip about 50% of the tests and is therefore not our final submission.

To test the implementation simply run:
> ghci: test_compareSumOfSquares
- +++ OK, passed 100 tests.
> ghci: test_compareSumOfCubes
- +++ OK, passed 100 tests.
--}

sumOfSquares :: Int -> Int
sumOfSquares n
  | n == 0 = 0
  | otherwise = n ^ 2 + sumOfSquares (n - 1)

sumOfSquaresFormula :: Int -> Int
sumOfSquaresFormula n = ((n * (n + 1)) * (2 * n +1)) `div` 6

compareSumOfSquares :: Positive Int -> Bool
compareSumOfSquares (Positive n) = sumOfSquares n == sumOfSquaresFormula n

sumOfCubes :: Int -> Int
sumOfCubes n
  | n == 0 = 0
  | otherwise = n ^ 3 + sumOfCubes (n - 1)

sumOfCubesFormula :: Int -> Int
sumOfCubesFormula n = ((n * (n+1)) `div` 2 ) ^ 2

compareSumOfCubes :: Positive Int -> Bool
compareSumOfCubes (Positive n) = sumOfCubes n == sumOfCubesFormula n

{-- Tests: By using verboseCheck we can actually see if QuickCheck also tests
for edge cases like n = 0.
 --}
test_compareSumOfSquares :: IO()
test_compareSumOfSquares = verboseCheck compareSumOfSquares

test_compareSumOfCubes :: IO()
test_compareSumOfCubes = verboseCheck compareSumOfCubes
