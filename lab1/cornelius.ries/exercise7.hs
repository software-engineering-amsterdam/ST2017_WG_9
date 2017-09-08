module Lab1 where
import Data.List
import Test.QuickCheck

 -- Time 3h

digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

calcValue :: Integer -> Integer
calcValue x
  | x * 2 > 9 = x*2-9
  | otherwise = x*2

calcEverySecond :: [Integer] -> [Integer]
calcEverySecond [] = []
calcEverySecond [x] = [x]
calcEverySecond (x:y:xs) = x : calcValue y : calcEverySecond xs

luhn :: Integer -> Bool
luhn i = sum(s) `mod` 10 == 0
  where d = digits i
        r = reverse d
        s = calcEverySecond r

startsWithS :: String -> String -> Bool
startsWithS [] [] = True
startsWithS _ [] = True
startsWithS [] _ = False
startsWithS (x:xs) (y:ys) = x == y && startsWithS xs ys

startsWith :: Integer -> Integer -> Bool
startsWith x y = startsWithS (show x) (show y)

checkPrefixes :: [Integer] -> Integer -> Bool
checkPrefixes [] _ = False
checkPrefixes (x:xs) n = startsWith n x || checkPrefixes xs n

checkLengths :: [Integer] -> Integer -> Bool
checkLengths l n = toInteger(length(digits n)) `elem` l

isAmericanExpress, isMaster, isVisa :: Integer -> Bool

isAmericanExpress n = checkLengths validLengths n && checkPrefixes validPrefixes n && luhn n
  where validLengths = [15]
        validPrefixes = [34,37]

isMaster n = checkLengths validLengths n && checkPrefixes validPrefixes n && luhn n
  where validLengths = [16]
        validPrefixes = [51,52,53,54,55] ++ [222100..272099]

isVisa n = checkLengths validLengths n && checkPrefixes validPrefixes n && luhn n
  where validLengths = [13,16,19]
        validPrefixes = [4]
