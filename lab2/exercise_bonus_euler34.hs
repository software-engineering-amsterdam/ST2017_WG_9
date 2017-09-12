module ExerciseBonusE34 where

{-
time: 30m

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
Find the sum of all numbers which are equal to the sum of the factorial of their digits.
Note: as 1! = 1 and 2! = 2 are not sums they are not included.

Result:
curiousNumbers [1..]
[145,40585^CInterrupted.
--> Not able to find another curious number after 40585 in an acceptable time range

-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

{--
digs: Splits a number into a list of digits
Taken from:
https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
-}
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

{------------------------------------------------------------------------------
                                    IMPLEMENTATION
-------------------------------------------------------------------------------}

curiousNumbers :: [Integer] -> [Integer]
curiousNumbers l = filter isCuriousNumber l

isCuriousNumber :: Integer -> Bool
isCuriousNumber n = n == sum (map factorial (digs n))

{------------------------------------------------------------------------------
                                    RUN
-------------------------------------------------------------------------------}

run :: [Integer]
run = curiousNumbers [3..]
