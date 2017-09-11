{--
  List A is a permutation of list B <=>
  length of A = length of B && all elements of A are in B && all elements of B are in A && order of A /= order of B
  [1,2,3] is a permutation of [3,2,1], but it's not its own permutation.
--}

{-- Time spent: 20m --}

{-- Notice: I had to strengthen the preconditions --}

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b
  | a == b = False
  | length a == length b && all (\ea -> ea `elem` b) a && all (\eb -> eb `elem` a) b = True
  | otherwise = False
