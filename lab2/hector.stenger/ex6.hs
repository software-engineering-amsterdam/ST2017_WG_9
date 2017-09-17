someIban = "DE44500105175407324931"
otherIban = "GB82WEST12345698765432"

iban :: [Char] -> Bool
iban n = number `mod` 97 == 1
    where ibanNumber = arrayToInteger (map convertToInteger (moveFourToBack n))
          number = read ibanNumber :: Integer

arrayToInteger :: [Int] -> [Char]
arrayToInteger [x] = show x ++ []
arrayToInteger (x:xs) = show x ++ arrayToInteger xs

convertToInteger :: Char -> Int
convertToInteger s
    | i < 58 && i > 47 = i - 48
    | otherwise = i - 55 
    where i = fromEnum s

moveFourToBack :: [Char] -> [Char]
moveFourToBack n = drop 4 n ++ take 4 n
