module BonusEuler9 where

ternaSums :: Int -> [(Int,Int,Int,Int)]
ternaSums x = [(a,b,c,a+b+c)|a<-[1..x], b<-[1..x], c<-[1..x], (a^2)+(b^2) == (c^2)]

ternaSums1000 :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
ternaSums1000 [] = []
ternaSums1000 ((_,_,_,s):xs)
  | s == 1000 = error "FOUND"
  | otherwise = ternaSums1000 xs
