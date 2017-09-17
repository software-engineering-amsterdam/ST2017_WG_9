import System.Random
import Test.QuickCheck

{--
  probs was given in the assignment - it generates a list of n random numbers
  in the range between 0 and 1
--}
probs :: Integer -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

{--
  quartileCount filters the input list and produces a list of four counters
  (buckets), that hold a number of elements in each quartile
--}
quartileCount :: IO [Float] -> IO [Int]
quartileCount ml = do
  l <- ml
  return [
    length $ filter (\x -> x<0.25) l,
    length $ filter (\x -> x>=0.25 && x<0.5) l,
    length $ filter (\x -> x>=0.5 && x<0.75) l,
    length $ filter (\x -> x>0.75) l
    ]

{-- The test --}

{--
  addMargin takes 3 arguments:
  - total number of elements (length of the initial list)
  - pieces - number of pieces that they divided into - in our case its always 4
  - param - a percentage (between 1 and 100) which increases resepectively the
    (pieces/n) calculation. It's used to provide an error margin for an
    acceptable number of elements in the 1/n subset (quartile in this case).
--}
addMargin :: Int -> Int -> Integer -> Float
addMargin total pieces param =
  (((fromIntegral total) / (fromIntegral pieces) ) * (1 + ((fromIntegral param) / 100)))

{--
  The final function - checkBalance - checks whether the list cm (wrapped in
  a monad here) contains element that are evenly distributed among the 4
  quartiles. The second argument - mp - is the margin percentage.
--}
checkBalance :: IO [Int] -> Integer -> IO Bool
checkBalance cm mp = do
  countings <- cm
  return (all (\x -> (fromIntegral x) < addMargin (sum countings) 4 mp) countings)

{--
  The final test run:
  - Check whether the probs list of 10000 elements is evenly distributed
    among quartiles with a 5% margin of error
--}
test = checkBalance (quartileCount $ probs 10000) 5 -- 5% margin rate
{--
  Results for this test:
  *Main> test
  True
  *Main>
  When tried multiple times it gives consistently True, which means that
  the number are evenly distributed among the quartiles with maximym
  a +/- 5% margin
--}

{--
  There can be performed a much better test - the checkBalance function
  can be run a few hunderd times with 1% margin, then 2, 3 and so on.
  The sum of successes of each loop would indicate how precise the probs
  generator is. I.e. it can tell that there is a 30% chance that the difference
  between the quartiles is smaller than 1%.
--}
