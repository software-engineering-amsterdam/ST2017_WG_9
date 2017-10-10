module Exercise1 where


-- | Exercise1
-- | ===========================================================================
-- | Time spent: 1h

import Data.List
import System.Random
import Math.NumberTheory.Logarithms
import Lecture6

-- | subpower of 2 = length of the list = integerLog2

-- | x^33 mod 5 = ((x^32 mod 5)*(x^1 mod 5)) mod 5
-- | \......../    \........../\........../ \..../
-- |   result        part1       part2     final mod
-- |
-- | explanation:
-- | number of recurrences: integerLog2 33 = 5
-- | what is left: 33-(2^5) = 33-32 = 1

-- | Proof for modular multiplication:
-- | https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/modular-multiplication

myExM :: Integer -> Integer -> Integer -> Integer
myExM _ 0 _ = 1
myExM x y n = (
      -- part1
      (pow2mod x (integerLog2 y) n)
      *
      -- part2
      -- (x^(y-2^(integerLog2 y)) `mod` n))
      (myExM x (y-2^(integerLog2 y)) n)
    )
    -- mod n
    `mod` n
    where
    pow2mod x i n -- | part1 returns: x^2^i mod n
      | i > 0 = pow2mod (x^2 `mod` n) (i-1) n
      | otherwise = x
