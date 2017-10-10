module Exercise5 where

-- | Exercise5
-- | ===========================================================================
-- | Time spent: 1h

import Data.List
import System.Random
import Lecture6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

-- | Carmichael number test
-- | Tests with Fermat's algorithm wether a carmichael number is prime (should
-- | return FALSE).
carmichaelTest :: Int -> Int -> IO ()
carmichaelTest k i = do
  let e = carmichael!!i
  res <- primeTestsF k e
  if res then
    print "primeTestsF failed"
  else
    print "primeTestsF passed"
  carmichaelTest k (i+1)

-- | By definition it is given, that Carmichael number is a composite number.
-- | Yet all Fermat prime tests fail for this numbers:
-- |
-- | Some test results:
-- |
-- | *Exercise5> carmichaelTest 1 0
-- | "primeTestsF failed"
-- | "primeTestsF failed"
-- | "primeTestsF failed"
-- | "primeTestsF failed"
-- | "primeTestsF failed"
-- | ...
-- |
-- | The reason is that, by nature, carmichael numbers are the wak point of
-- | Fermat's algorithm, after Wikipedia (https://en.wikipedia.org/wiki/Carmichael_number):
-- | "Fermat's little theorem states that if p is a prime number, then for any
-- | integer b, the number b^p − b is an integer multiple of p. Carmichael
-- | numbers are composite numbers which have this property."
