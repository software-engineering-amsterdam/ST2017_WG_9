import Data.List
import Test.QuickCheck

{--
  List A is a permutation of list B <=>
  length of A = length of B && all elements of A are in B && all elements of B
  are in A && order of A /= order of B.
  [1,2,3] is a permutation of [3,2,1], but it's not its own permutation.
--}

{-- Time spent: 2h --}

{-- Some predefined functions--}

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

{-- Notice: I had to strengthen the preconditions --}

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b
  | a == b = False
  | length a == length b && all (\ea -> ea `elem` b) a && all (\eb -> eb `elem` a) b = True
  | otherwise = False

{-- Rotate by n positions to the right --}
rotate :: [Int] -> Int -> [Int]
rotate l n = drop n l ++ take n l

{-- Some testable permutations --}
{--
  In the assignment we were allowed to assume that the properties may contain
  no duplicates. This has a huge implact on the problem, as a full implementation
  of isPermutation should actually count the number of ocurrences of duplicates
  and their indexes. I.e. [1,2,2] and [1,2,2] can theoreticaly be each other
  permutations if in the second list the two "2"s were swaped. To know that we
  would have to identify them somehow as differen objects. Since we work here
  on natural numbers only and we don't keep a track of items idexes or memory
  addresses, it is rather impossible to detect that.
--}

baseList = [1..10]
testList0 = baseList           -- should fail
testList1 = reverse baseList   -- should succeed
testList2 = rotate baseList 5  -- should succeed
testList3 = take 9 baseList    -- should fail
testList4 = drop 9 baseList    -- should fail
testList5 = []                 -- should fail

{-- Testing the lists --}
listTest0 = isPermutation baseList testList0
listTest1 = isPermutation baseList testList1
listTest2 = isPermutation baseList testList2
listTest3 = isPermutation baseList testList3
listTest4 = isPermutation baseList testList4
listTest5 = isPermutation baseList testList5

{-- Results as expected:
*Main> listTest0
False
*Main> listTest1
True
*Main> listTest2
True
*Main> listTest3
False
*Main> listTest4
False
*Main> listTest5
False
--}

{-- Properties for the exersise  --}
domain = [-10..10] ++ [1,2,3]

-- one precondition is that the function acceppts any list
precondition1 :: [Integer] -> Bool
precondition1 _ = True

-- another precondition is that the entry set holds no duplicates
precondition2 :: [Integer] -> Bool
precondition2 [] = True
precondition2 (x:xs) = not (x `elem` xs) && precondition2 xs

{-- by using the compar function: --}
compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

{--
  we clearly se, that for a domain holding doubles,
  precondition1 i weaker than precondition2
--}
compareP1withP2 = compar [domain] precondition1 precondition2
{--
  *Main> compareP1withP2
  "weaker"
--}

{--
  for domains holding no doubles, the preconditions would be equivalent:
--}

compare2P1withP2 = compar [[1..100]] precondition1 precondition2
{--
  *Main> compare2P1withP2
  "equivalent"
--}

{--
  An ordered list of Properties (descending - starting from the strongest):
  precondition2
  precondition1
--}

{--
  The test proces can be automatized but it requires using monads and wrapping
  everythin up in them.

  I've prepared a test of reversed sets and sets rotated by 1 element to the right:

--}

reverseTest :: Int -> Bool -- reversed list of 1..n are always a permutation
reverseTest n = isPermutation [1..n] $ reverse [1..n]
automaticTest1 = quickCheckResult (\n -> n > 2 --> reverseTest n) -- this test should always succeed

rotateTest :: Int -> Bool
rotateTest n = isPermutation [1..n] (rotate [1..n] 1) -- rotating a list by 1 to the right
automaticTest2 = quickCheckResult (\n -> n > 2 --> rotateTest n) -- this test should always succeed

{--
  The results are:

  *Main> automaticTest1
  +++ OK, passed 100 tests.
  Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
  *Main> automaticTest2
  +++ OK, passed 100 tests.
  Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
  *Main>

--}
