import Data.Char

{-- Implementing and testing IBAN validation --}

ibanLenghtByCountry = [("AL",28),("AD",24),("AT",20),("AZ",28),("BH",22),("BY",28),("BE",16),("BA",20),("BR",29),("BG",22),("CR",22),("HR",21),("CY",28),("CZ",24),("DK",18),("DO",28),("SV",28),("EE",20),("FO",18),("FI",18),("FR",27),("GE",22),("DE",22),("GI",23),("GR",27),("GL",18),("GT",28),("HU",28),("IS",26),("IQ",23),("IE",22),("IL",23),("IT",27),("JO",30),("KZ",20),("XK",20),("KW",30),("LV",21),("LB",28),("LI",21),("LT",20),("LU",20),("MK",19),("MT",31),("MR",27),("MU",30),("MD",24),("MC",27),("ME",22),("NL",18),("NO",15),("PK",24),("PS",29),("PL",28),("PT",25),("QA",29),("RO",24),("LC",32),("SM",27),("ST",25),("SA",24),("RS",22),("SC",31),("SK",24),("SI",19),("ES",24),("SE",24),("CH",21),("TL",23),("TN",24),("TR",26),("UA",29),("AE",23),("GB",22),("VG",24)]

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
