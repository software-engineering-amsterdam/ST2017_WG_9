import Data.List

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

forall = flip all

isDerangement :: [Int] -> [Int] -> Bool
isDerangement [] [] = True
isDerangement _ [] = False
isDerangement [] _ = False
isDerangement [x] [y] = not (x == y)
isDerangement (x:xs) (y:ys)
    | x == y = False
    | elem x ys = False
    | otherwise = isDerangement xs ys

deran :: [Int] -> [[Int]]
deran n = filter (isDerangement n) $ permutations n

samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

sameitems :: [Int] -> [Int] -> Bool
sameitems [] [] = True
sameitems [x] ys = elem x ys
sameitems (x:xs) ys = elem x ys && sameitems xs ys

samelists :: [Int] -> [Int] -> Bool
samelists x y = x == y

lengthCheck :: [Int] -> [Int] -> Bool
lengthCheck x y = (isDerangement x y) --> samelength x y

itemsCheck :: [Int] -> [Int] -> Bool
itemsCheck x y = (isDerangement x y) --> sameitems x y

listsCheck :: [Int] -> [Int] -> Bool
listsCheck x y = (isDerangement x y) --> samelength x y
