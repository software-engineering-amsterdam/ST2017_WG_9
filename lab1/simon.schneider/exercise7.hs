module Exercise7 where

luhnMultiplyByTwo :: Int -> Int


luhnCheck :: [Int] -> Bool
luhnCheck l
  | length l != 16 = false
  otherwise = true
    where
      evenList = filter even l
      oddSum = sum filter odd l
      evenSum = foldr (\sum j -> acc + x) 0 xs
