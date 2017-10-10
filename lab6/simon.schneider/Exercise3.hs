module Exercise3 where

{------------------------------------------------------------------------------
time spent: 10m

Task:
Fermats primality check has false positives, primes are always identified as
primes, but composites are sometimes labeled as primes
Write a function composites :: [Integer] that generates the infinite list of
composite natural numbers

Answer:
See composites' for infinite,
compositesFinite for finite list:

*Exercise3> compositesFinite 10
[1,4,6,8,9,10]

-------------------------------------------------------------------------------}

import Lecture6

-- One is neither prime nor composite
isComposite :: Integer -> Bool
isComposite n = not (prime n) && n /= 1

composites' :: [Integer]
composites' = filter isComposite [0..]

compositesFinite :: Integer -> [Integer]
compositesFinite n = filter isComposite [0..n]

compositesStartingAt :: Integer -> [Integer]
compositesStartingAt n = filter isComposite [n..]
