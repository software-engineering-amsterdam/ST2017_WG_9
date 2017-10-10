module Exercise5 where

{------------------------------------------------------------------------------
Time spent: 1h

TASK:
Use the list generated by the following function for a further test of Fermat's
primality check.

-------------------------------------------------------------------------------}

import Lecture6
import Exercise3

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
  k <- [2..],
  prime (6*k+1),
  prime (12*k+1),
  prime (18*k+1)]

findFalsePositives' :: Int -> [Integer] -> IO [Integer]
findFalsePositives' _ [] = return []
findFalsePositives' k (c:cs) =
  do
    failed <- primeTestsF k c
    if failed then
      do
        -- putStrLn ("FOUND FALSE POSITIVE: " ++ show c)
        t <- findFalsePositives' k cs
        return (c : t)
    else
      findFalsePositives' k cs

testCarMichael k n = findFalsePositives' k (take n carmichael)
testComposites k n = findFalsePositives' k (take n composites')
