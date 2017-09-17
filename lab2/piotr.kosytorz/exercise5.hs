import Data.List

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

checkInitialProperties :: [Integer] -> [Integer] -> Bool
checkInitialProperties l1 l2
  | l1 == l2 = False
  | length l1 /= length l2 = False
  | all (\x -> x `elem` l1) l2 = True
  | otherwise = False

isDerangementHlp :: Eq a => [a] -> [a] -> Bool
isDerangementHlp [] [] = True
isDerangementHlp l1@(x:xs) l2@(y:ys)
  | x /= y = isDerangementHlp xs ys
  | otherwise = False

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement l1 l2 = checkInitialProperties l1 l2 && isDerangementHlp l1 l2

deran :: Integer -> [[Integer]]
deran n = filter (\x -> isDerangement x [0..(n-1)]) (permutations [0..(n-1)])

{-- Test properties --}

sameList :: [Integer] -> [Integer] -> Bool
sameList l1 l2 = l1 == l2

sameLength :: [Integer] -> [Integer] -> Bool
sameLength l1 l2 = length l1 == length l2

list1inlist2 :: [Integer] -> [Integer] -> Bool
list1inlist2 l1 l2 = all (\x -> x `elem` l1) l2

-- l2 is a permutatiom of l1
isPermutation :: [Integer] -> [Integer] -> Bool
isPermutation l1 l2 = l2 `elem` (permutations l1)

listHasOnlyUniqueElements :: [Integer] -> Bool
listHasOnlyUniqueElements [] = True
listHasOnlyUniqueElements (x:xs) = x `notElem` xs && listHasOnlyUniqueElements xs

testSwap :: [Integer] -> [Integer] -> Bool
testSwap l1 l2 = isDerangement l1 l2 && isDerangement l2 l1

{-- TODO: Orderd list of properties --}

{-- Test lists for testing  --}

-- should pass: rotated list
testList11 = [1,2,3,4,5]
testList12 = [2,3,4,5,1]

-- should pass: reversed list of an even length
testList21 = [1,2,3,4,5,6]
testList22 = [6,5,4,3,2,1]

-- should fail: reversed list of an odd length
testList31 = [1,2,3,4,5,6,7]
testList32 = [7,6,5,4,3,2,1]

-- should fail: uneven length
testList41 = [1,2,3,4,5,6,7]
testList42 = [7,6,5,3,2,1]

-- should fail: empty lists
testList51 = []
testList52 = []

-- should fail: list of only one element
testList61 = [1]
testList62 = [1]

-- should fail: lists containg a double instance of an element
testList71 = [1,2,3,3,4]
testList72 = [3,1,4,2,3]

{--
  isDerangement testList11 testList12
  isDerangement testList21 testList22
  isDerangement testList31 testList32
  isDerangement testList41 testList42
  isDerangement testList51 testList52
  isDerangement testList61 testList62
  isDerangement testList71 testList72
--}
