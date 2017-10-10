module Exercise4 where

-- | Exercise4
-- | ===========================================================================
-- | Time spent: 1h - due to the cool tests that I made ;)

import Data.List
import System.Random
import Lecture6

-- | Fool test
-- | params:
-- | k - number of iterations for Fermat test
-- | i - index of the the element from the composites list
foolTest :: Int -> Int -> IO Integer
foolTest k i = do
  let e = composites!!i    -- | take n-th element of the list of composits
  res <- primeTestsF k e   -- | test it in k iterations
  if res
    then return e          -- | if TRUE then the Fermat test was fooled
    else foolTest k (i+1)  -- | if false then go for next element in the list of composits

-- | A helper for tesing
-- | k - number of iterations for Fermat test
-- | i - index of the the element from the composites list
-- | n - number of tests
-- | r - actual minimum
smallestFalsePrimeAfterNTests k i n r = do
  testRes <- foolTest k i
  if n>0 then
    if r>testRes then
      smallestFalsePrimeAfterNTests k i (n-1) testRes
    else
      smallestFalsePrimeAfterNTests k i (n-1) r
  else return r

-- | Tests:
-- | I've performed the test on with a number of variables, and what's the most
-- | surprising is, that for 1000 tests, the k didn't improve efficiency of the
-- | Fermat's algorithm. It got fooled by 9 for k=1,2,3. At the bottom I present
-- | some more runs for the test (with 100 tests per a set of parameters) - that
-- | produced different results, but the minimum that fools the test is so to
-- | see 9.
-- | ===========================================================================
-- |
-- | k = 1
-- |
-- | Let's take k=1 and let's run 1000 tests
-- | r = 2^64 - a fairly big int that the test should not reach
-- |
-- | Results:
-- | *Exercise4> smallestFalsePrimeAfterNTests 1 0 1000 (2^64)
-- | 9
-- | (0.63 secs, 374,971,888 bytes)
-- |
-- | Comment: After 1000 tests, for k=1 the smallest number that has folled the
-- | Fermat test is 9
-- | ===========================================================================
-- |
-- | k = 2
-- |
-- | Let's take k=2 and let's run 1000 tests
-- | r = 2^64 - a fairly big int that the test should not reach
-- |
-- | Results:
-- | *Exercise4> smallestFalsePrimeAfterNTests 2 0 1000 (2^64)
-- | 9
-- | (14.47 secs, 10,221,680,920 bytes)
-- |
-- | Comment: After 1000 tests, for k=2 the smallest number that has folled the
-- | Fermat test is also 9!
-- | ===========================================================================
-- |
-- | k = 3
-- |
-- | Let's take k=3 and let's run 1000 tests
-- | r = 2^64 - a fairly big int that the test should not reach
-- |
-- | Results:
-- | *Exercise4> smallestFalsePrimeAfterNTests 3 0 1000 (2^64)
-- | 9
-- | (80.62 secs, 53,167,118,952 bytes)
-- |
-- | Comment: After 1000 tests, for k=3 the smallest number that has folled the
-- | Fermat test is also 9!!
-- | ===========================================================================
-- |
-- | Some more test runs:
-- |
-- | k=1
-- |
-- | *Exercise4> smallestFalsePrimeAfterNTests 1 0 100 (2^64)
-- | 9
-- | (0.17 secs, 49,410,848 bytes)
-- |
-- | k=2
-- |
-- | *Exercise4> smallestFalsePrimeAfterNTests 2 0 100 (2^64)
-- | 15
-- | (1.58 secs, 1,004,703,040 bytes)
-- |
-- | k=3
-- |
-- | *Exercise4> smallestFalsePrimeAfterNTests 3 0 100 (2^64)
-- | 15
-- | (8.58 secs, 5,967,480,488 bytes)
-- | *Exercise4> smallestFalsePrimeAfterNTests 3 0 100 (2^64)
-- | 9
-- | (7.26 secs, 5,135,832,072 bytes)
-- | *Exercise4> smallestFalsePrimeAfterNTests 3 0 100 (2^64)
-- | 21
-- | (6.65 secs, 4,793,023,536 bytes)
-- |
