import Test.QuickCheck

asciiUpper :: Int
asciiUpper = 122

asciiLower :: Int
asciiLower = 97

inAsciiRange :: Int -> Bool
inAsciiRange c = c >= asciiLower && c <= asciiUpper

asciiTurnover :: Int -> Int
asciiTurnover n
    | n > asciiUpper = n `mod` asciiUpper + 96
    | otherwise = n

substitute :: Int -> Char -> Char
substitute m n
    | inAsciiRange asciiValue = toEnum (asciiTurnover ((fromEnum n) + m))
    | otherwise = n
    where asciiValue = fromEnum n

rot :: Int -> [Char] -> [Char]
rot s n = map (\x -> substitute s x) n

sameLength :: [Char] -> Bool
sameLength s = length (rot 13 s) == length s

substituteNonAscii :: [Char] -> Bool
substituteNonAscii s = rot 13 nonAscii == nonAscii
    where nonAscii = filter (\n -> not $ inAsciiRange $ fromEnum n) s

doubleSubstitute :: [Char] -> Bool
doubleSubstitute s = rot 13 (rot 13 s) == s 
