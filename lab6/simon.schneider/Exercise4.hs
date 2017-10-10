module Exercise4 where

import Lecture6Dirty
import Exercise3

{------------------------------------------------------------------------------
time spent: 30m


TASK 1:
What is the least composite number that you can find that fools the check
 for primeTestsF k with 1,2,3 ?
 --> Note for teacher: prime_tests_F (question) was renamed to primeTestsF (code)

 *Exercise4> test 1 2000
 FOUND FALSE POSITIVE: 25
 FOUND FALSE POSITIVE: 33
 FOUND FALSE POSITIVE: 75
 FOUND FALSE POSITIVE: 171
 FOUND FALSE POSITIVE: 175
 FOUND FALSE POSITIVE: 253
 FOUND FALSE POSITIVE: 280
 FOUND FALSE POSITIVE: 341
 FOUND FALSE POSITIVE: 425
 FOUND FALSE POSITIVE: 511
 FOUND FALSE POSITIVE: 559
 FOUND FALSE POSITIVE: 561
 FOUND FALSE POSITIVE: 628
 FOUND FALSE POSITIVE: 861
 FOUND FALSE POSITIVE: 949
 FOUND FALSE POSITIVE: 1105
 FOUND FALSE POSITIVE: 1519
 FOUND FALSE POSITIVE: 1705
 END.

*Exercise4> test 2 2000
FOUND FALSE POSITIVE: 49
FOUND FALSE POSITIVE: 861
FOUND FALSE POSITIVE: 1105
END.

*Exercise4> test 3 2000
FOUND FALSE POSITIVE: 1729
END.

Finding 1: The amount of false positives decreases if k is increased
Finding 2: The false positives are not deterministic. `test1 2000` returns a
diffrent result for each run.

TASK 2:
What happens if you increase k?

The amount of false positives is reduced.

*Exercise4> test 1 20000
71

*Exercise4> test 2 20000
14

*Exercise4> test 3 20000
4

*Exercise4> test 4 20000
2

*Exercise4> test 5 20000
3

*Exercise4> test 6 20000
2

*Exercise4> test 7 20000
2
...........

*Exercise4> test 15 20000
0
*Exercise4> test 15 20000
0
.........
*Exercise4> test 15 20000
0
.........
*Exercise4> test 20 20000
0

A value of k larger than 3 does not seem to improve the accuracy much further at
first. But the jump from 7 to 15 improved the accuracy to a seemingly perfect
result. k increases the amount of tests and therefore also the needed runtime.
If someone uses the algorithm he has to weight performance against accuracy.

-------------------------------------------------------------------------------}

findFalsePositives' :: Int -> [Integer] -> IO [Integer]
findFalsePositives' _ [] = return []
findFalsePositives' k (c:cs) =
  do
    failed <- primeTestsF k c
    if failed then
      do
        putStrLn ("FOUND FALSE POSITIVE: " ++ show c)
        t <- findFalsePositives' k cs
        return (c : t)
    else
      findFalsePositives' k cs

test :: Int -> Integer -> IO Int
test k n = do
  ps <- findFalsePositives' k (compositesFinite n)
  return (length ps)

--TODO: Find smallest false positive
