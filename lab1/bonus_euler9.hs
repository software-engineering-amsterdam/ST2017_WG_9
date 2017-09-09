module BonusEuler9 (run) where

{-
Time: 60m

https://projecteuler.net/problem=9:
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

-}

{-
ternaSums function inspired by :
https://stackoverflow.com/questions/7261667/pythagorean-triple-in-haskell-without-symmetrical-solutions
-}

{-
First try: O(n^3), where n is instances of
ternaSums3(1000) done after 1.000.000.000 iterations
-}
ternaSums1 :: Int -> [(Int,Int,Int,Int)]
ternaSums1 x = [(a,b,c,a+b+c) | a<-[1..x], b<-[1..x], c<-[1..x], (a^2)+(b^2) == (c^2) && a + b + c == 1000]

{-
Second try: Much faster because we only have to generate a and b O(n^2)
ternaSums2(1000) done after 1.000.000 iterations
-}
ternaSums2 :: Int -> [(Int,Int,Int,Int)]
ternaSums2 x = [(a,b,(1000 - a - b),a + b + (1000 - a - b)) | a<-[1..x], b<-[1..x], (a^2)+(b^2) == ((1000 - a - b)^2)]

{-
Third try: Even faster than version 2 because we do not iterate from 0 to 1000
We use the knowledge that c is equal to 1000 - a - b (because a+b+c = 1000)
a iterates from [1..333]
and b iterates from [1..500]
ternaSums3(1000) done after (333*500) = 166.500 iterations

Inspired by imperative solution: http://www.mathblog.dk/pythagorean-triplets/
-}
ternaSums3 :: Int -> [(Int,Int,Int,Int)]
ternaSums3 x = [(a,b,(1000 - a - b),a + b + (1000 - a - b)) | a<-[1..(x `div` 3)], b<-[1..(x `div` 2)], (a^2)+(b^2) == ((1000 - a - b)^2)]

{- Result: (200,375,425,1000) -}
run :: (Int,Int,Int,Int)
run = head(ternaSums3 1000)
