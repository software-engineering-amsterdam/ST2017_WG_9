module Exercise2 where

-- | Exercise1
-- | ===========================================================================
-- | Time spent: 30m

import Data.List
import System.Random
import Math.NumberTheory.Logarithms
import Lecture6
import Exercise1

-- | very small numbers
test_1_v1 = expM 7 33 4
test_1_v2 = myExM 7 33 4
-- | results:
-- | *Exercise2> test_1_v1
-- | 3
-- | (0.01 secs, 78,832 bytes)
-- | *Exercise2> test_1_v2
-- | 3
-- | (0.00 secs, 85,560 bytes)

-- | some bigger numbers
test_2_v1 = expM 11 1232345 71
test_2_v2 = myExM 11 1232345 71
-- | *Exercise2> test_2_v1
-- | 34
-- | (0.04 secs, 1,641,928 bytes)
-- | *Exercise2> test_2_v2
-- | 34
-- | (0.00 secs, 213,904 bytes)

-- | even bigger numbers
test_3_v1 = expM 17 107311111 13
test_3_v2 = myExM 17 107311111 13
-- | *Exercise2> test_3_v1
-- | 4
-- | (5.08 secs, 148,939,352 bytes)
-- | *Exercise2> test_3_v2
-- | 4
-- | (0.00 secs, 265,384 bytes)

-- | even bigger numbers
test_4_v1 = expM 1997 10737419 11
test_4_v2 = myExM 1997 10737419 11
-- | *Exercise2> test_4_v1
-- | Segmentation fault: 11
-- | *Exercise2> test_4_v2
-- | 2
-- | (0.01 secs, 255,320 bytes)

-- | very big numbers
test_5_v1 = expM 2719 1152921504606846981 1997
test_5_v2 = myExM 2719 1152921504606846981 1997
-- | *Exercise2> test_5_v1
-- | -- killed my computer
-- | *Exercise2> test_5_v2
-- | 1816
-- | (0.00 secs, 81,976 bytes)

-- | Result: The new implementation is way more efficient.
