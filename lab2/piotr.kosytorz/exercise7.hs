import Data.Char

{-- Implementing and testing IBAN validation --}

removeSpaceAndToUpperCase :: String -> String
removeSpaceAndToUpperCase s = map toUpper (filter (/= ' ') s)

rearrange :: String -> String
rearrange s = (drop 4 s) ++ (take 4 s)

replaceLettersWithNumbers :: String -> String
replaceLettersWithNumbers [] = []
replaceLettersWithNumbers (x:xs)
  | ord x >= 65 && ord x <= 90 = show (ord x - 55) ++ replaceLettersWithNumbers xs
  | otherwise = x:replaceLettersWithNumbers xs

str2integer :: String -> Integer
str2integer s = read s :: Integer

checkReminder :: Integer -> Bool
checkReminder n = n `mod` 97 == 1

iban :: String -> Bool
iban s
  | length s < 15 && length s > 32 = False
  | checkReminder $ str2integer $ replaceLettersWithNumbers $ rearrange $ removeSpaceAndToUpperCase s = True
  | otherwise = False
