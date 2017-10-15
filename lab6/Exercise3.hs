module Exercise3 where

{------------------------------------------------------------------------------
time spent: 15m

Task:
Fermats primality check has false positives, primes are always identified as
primes, but composites are sometimes labeled as primes
Write a function composites :: [Integer] that generates the infinite list of
composite natural numbers

Answer:
See composites' for infinite,
compositesFinite for finite list:

*Exercise3> compositesFinite 20
[4,6,8,9,10,12,14,15,16,18,20]

*Exercise3> primes
[2,3,5,7,11,13,17,19...]

Every integer is composite as long as it is not a prime number. However, there is
one exception to this rule. The number 1 is not a prime number (does not have two
diffrent divisors) nor a composite (a whole number that can be divided by numbers
other than 1 or itself)

Example:
*Exercise3> sort((takeWhile (<=20) primes) ++ compositesFinite 20 ++ [1]) == [1..20]
True

-------------------------------------------------------------------------------}

import Lecture6

import Data.List

-- One is neither prime nor composite
isComposite :: Integer -> Bool
isComposite n = not (prime n) && n /= 1

composites' :: [Integer]
composites' = filter isComposite [0..]

compositesFinite :: Integer -> [Integer]
compositesFinite n = filter isComposite [0..n]

compositesStartingAt :: Integer -> [Integer]
compositesStartingAt n = filter isComposite [n..]
