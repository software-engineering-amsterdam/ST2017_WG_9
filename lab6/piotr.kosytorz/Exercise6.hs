module Exercise6 where

-- | Exercise6
-- | ===========================================================================
-- | Time spent: 10m

import Data.List
import System.Random
import Lecture6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

-- | Miller-Robin number test
mrTest :: Int -> Int -> IO ()
mrTest k i = do
  let e = carmichael!!i
  res <- primeMR k e -- | using primeMR from the Lecture6 code
  if res then
    print "MR test failed"
  else
    print "MR test passed"
  mrTest k (i+1)

-- | Q: What do you find?
-- |
-- | A: For carmichael numbers, MR test performs much better than Fermat's test,
-- | giving most of the times good results:
-- |
-- | *Exercise6> mrTest 1 0
-- | "MR test passed"
-- | "MR test passed"
-- | "MR test passed"
-- | "MR test passed"
-- | ...
