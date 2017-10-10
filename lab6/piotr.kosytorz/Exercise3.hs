module Exercise3 where

-- | Exercise3
-- | ===========================================================================
-- | Time spent: 10m

import Data.List
import System.Random
import Lecture6

composites' :: [Integer]
composites' = filter (not.prime) [2..]
