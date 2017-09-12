module Exercise7 where

import Data.List
import Data.Char

{--


1 Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
2 Move the four initial characters to the end of the string
3 Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
4 Interpret the string as a decimal integer and compute the remainder of that number on division by 97
Example:
• IBAN:		GB82WEST12345698765432
• Rearrange:		WEST12345698765432GB82
• Convert to integer:		3214282912345698765432161182
• Compute remainder:		3214282912345698765432161182	mod 97 = 1
Source: https://en.wikipedia.org/wiki/International_Bank_Account_Number
--}

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

takeLastN :: Int -> [a] -> [a]
takeLastN n xs = drop (length xs - n) xs

preprocessIban :: String -> String
preprocessIban s = map toUpper (filter (/= ' ') s)

iban :: String -> Bool
iban s = n `mod` 97 == 1
  where
    s1 = preprocessIban s
    rearr = ibanRearrange s1
    converted = foldl (\a c -> a ++ ibanConvertCharToNumber c) "" rearr
    n = read converted :: Integer

ibanRearrange :: String -> String
ibanRearrange s = drop 4 s ++ take 4 s

ibanConvertCharToNumber :: Char -> String
ibanConvertCharToNumber c
  | i >= 10 && i <= 35 = show i
  | otherwise = [c]
  where i = fromEnum c - 55

validCards :: [String]
validCards = [
  "AL47 2121 1009 0000 0002 3569 8741",
  "AD12 0001 2030 2003 5910 0100",
  "AT61 1904 3002 3457 3201",
  "AZ21 NABZ 0000 0000 1370 1000 1944",
  "BH67 BMAG 0000 1299 1234 56",
  "BE62 5100 0754 7061",
  "BA39 1290 0794 0102 8494",
  "BG80 BNBG 9661 1020 3456 78",
  "HR12 1001 0051 8630 0016 0",
  "CY17 0020 0128 0000 0012 0052 7600",
  "CZ65 0800 0000 1920 0014 5399",
  "DK50 0040 0440 1162 43",
  "EE38 2200 2210 2014 5685",
  "FO97 5432 0388 8999 44",
  "FI21 1234 5600 0007 85"]

testValidCards :: Bool
testValidCards = all iban validCards

invalidCards :: [String]
invalidCards = filter (\c -> preprocessIban c /= preprocessIban(head validCards)) cs
  where
    cs = take 50 (permutations (head validCards))

findValidPermutations :: [(Bool, String)]
findValidPermutations = filter (\c -> (iban c, c)) invalidCards

{--TODO: Find tolerance of permutated iban numbers --}
