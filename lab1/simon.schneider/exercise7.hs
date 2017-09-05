module Exercise7 where

import Data.Char

stringToDigits :: String -> [Int]
stringToDigits = map digitToInt

filterByIndex :: (Int -> Bool) -> [a] -> Int -> [a]
filterByIndex _ [] _ = []
filterByIndex f (x:xs) s
  | f s = x : filterByIndex f xs (s+1)
  | otherwise = filterByIndex f xs (s+1)

luhnMultiplyByTwo :: Int -> Int
luhnMultiplyByTwo x
  | x * 2 < 10 = x * 2
  | otherwise = (x*2) `mod` 10 + 1

luhnCheck :: [Int] -> Bool
luhnCheck l
  | length l < 15 = False
  | otherwise = (oddSum + evenSum) `mod` 10 == 0
    where
      evenList = filterByIndex even l 0
      oddSum = sum (filterByIndex odd l 0)
      evenSum = foldl (\s j -> s + luhnMultiplyByTwo j ) 0 evenList

luhnCheckString :: String -> Bool
luhnCheckString l = luhnCheck ( map digitToInt l )

isVisa :: [Int] -> Bool
isVisa l@(x:_) = luhnCheck l &&
  (length l == 15 || length l == 13) &&
  x == 4
isVisa _ = False

isAmericanExpress :: [Int] -> Bool
isAmericanExpress l@(x1:x2:_) = luhnCheck l &&
  (length l == 15 ) &&
  x1 == 3 && (x2 == 4 || x2 == 7)
isAmericanExpress _ = False


isMaster :: [Int] -> Bool
isMaster l@(x1:x2:_) =
    x1 == 5 && x2 `elem` [1..5] &&
    length l == 16 &&
    luhnCheck l
isMaster _ = False

visaCards :: [String]
visaCards = [
  "4929678497491046",
  "4716530484108025",
  "4916255259323538",
  "4716362200888791",
  "4539814033273950"
  ]

masterCards :: [String]
masterCards = [
  "5123938634489779",
  "5495255942952004",
  "5372486713140234",
  "5291647544638336",
  "5151073727318415"
  ]

americanExpressCards :: [String]
americanExpressCards = [
  "376731710065901",
  "340709209797559",
  "349463117583724",
  "371857926785613",
  "345112805596266"
  ]

visaCardsInvalid :: [String]
visaCardsInvalid = [
  "4929698497491046",
  "4716530474108025",
  "4916255259323538",
  "4716360888791",
  "6539814033273950"
  ]

masterCardsInvalid :: [String]
masterCardsInvalid = [
  "5723938634489779",
  "5495855942952004",
  "5372889713140234",
  "5291640544638336",
  "5151071727318415"
  ]

americanExpressCardsInvalid :: [String]
americanExpressCardsInvalid = [
  "376738710065901",
  "340709209797559",
  "349461117583724",
  "371855926785613",
  "3451128055266"
  ]

testManyCards :: [String] -> ([Int] -> Bool) -> Bool
testManyCards l f = all (f . stringToDigits) l
