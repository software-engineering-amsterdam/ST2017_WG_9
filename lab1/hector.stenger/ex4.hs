import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

reversal :: Integer -> Integer
reversal = read . reverse . show

isPrime :: Integer -> Bool
isPrime n = not (any (\x -> n `mod` x == 0) [2..(n-1)])

predicate :: Integer -> Bool
predicate n = n > 0 && isPrime n && isPrime (reversal n)

findPrimesIn1000 :: [Integer]
findPrimesIn1000 = filter predicate [1..1000]

runner = verboseCheck(\n -> predicate n --> elem n findPrimesIn1000)
