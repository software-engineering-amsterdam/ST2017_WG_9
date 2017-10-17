module Workshop6 where

import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
  (Leaf "Turing, Alan"))
  (Leaf "Goedel, Kurt")

{------------------------------------------------------------------------------
Question 1: Implement leafCount and test for correctness
-------------------------------------------------------------------------------}
leafCount :: Blt a -> Int
leafCount (Leaf _) = 1
leafCount (Node a b) = leafCount a + leafCount b

{--
Base case: Blt a = Leaf a
leafCount (Leaf a) = 1            [Check]

Induction hypothesis: leafCount Blt a + leafCount Blt b = leafCount Node (Blt a) (Blt b)

Induction step:
Add one leaf (Leaf n) to existing Blt
--}

{------------------------------------------------------------------------------
Question 2: Define mapB that does for binary trees what map does for lists.
-------------------------------------------------------------------------------}

mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf a) = Leaf (f a)
mapB f (Node a b) = Node (mapB f a) (mapB f b)

{------------------------------------------------------------------------------
Question 3: counts the number of nodes of a tree.
-------------------------------------------------------------------------------}

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

count :: Tree a -> Int
count (T _ ts) = foldl (\a t -> a + count t) 1 ts

{------------------------------------------------------------------------------
Question 4: give the depth of a Tree
-------------------------------------------------------------------------------}

depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = 1 + foldl (\a t -> max a (depth t)) 0 ts

{--
Proof:

Base case: depth (Tree a []) = 0                        [Check]

Induction hypotheses holds for subtrees:
i.H: depth of tree with subtrees is 1 + maximum of subtrees

--}

{------------------------------------------------------------------------------
Question 5: function mapT that does for trees what map does for lists.
-------------------------------------------------------------------------------}
mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T a ts) = T b ts'
  where
    b = f a
    ts' = map (mapT f) ts

mapT' :: (a -> b) -> Tree a -> Tree b
mapT' f (T a ts) = T (f a) (map (mapT' f) ts)

{------------------------------------------------------------------------------
Question 6: How can you test mapT from the previous question for correctness?
Or can you perhaps prove that it is correct?
-------------------------------------------------------------------------------

Base case: base case xs == []

Induction Hypothesis: (mapT f) x:xs

-------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
Question 7: Write a function collect that collects the information in a tree of
type Tree a in a list of type [a].
-------------------------------------------------------------------------------}

collect :: Tree a -> [a]
collect (T a ts) = [a] ++ foldl (\c t -> c ++ collect t) [] ts


{------------------------------------------------------------------------------
Question 8: Redifine count, depth, collect and mapT f in terms of foldT.
-------------------------------------------------------------------------------}
foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

count' :: Tree a -> Int
count' t = foldT (\_ bs -> 1 + sum bs) t

depth' :: Tree a -> Int
depth' t = foldT (\_ bs -> 1 + foldl max 0 bs) t - 1

collect' :: Tree a -> [a]
collect' t = foldT (\a bs -> (a : concat bs)) t

mapT'' :: (a -> b) -> Tree a -> Tree b
mapT'' f t = foldT (\a ts -> T (f a) ts) t

{------------------------------------------------------------------------------
Question 9: Grow and predict count
-------------------------------------------------------------------------------}

grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

growCount1, growCount2 :: Int
growCount1 = count (grow (\x -> if x < 2 then [x+1, x+1] else []) 0) -- =7
growCount2 = count (grow (\x -> if x < 6 then [x+1, x+1] else []) 0) -- =127

{--
Predict length
2^0+2^1+2^2= 7
2^0+2^1+2^2+2^3+2^4+2^5+2^6 = 2^7 -1 = 127
--}

{------------------------------------------------------------------------------
Question 10: Infinite trees
-------------------------------------------------------------------------------}
infTree :: Tree Integer
infTree = grow (\ n -> [n+1,n+1]) 0

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x (map (takeT (n-1)) ts)

{------------------------------------------------------------------------------
Question 11: Infinite trees
-------------------------------------------------------------------------------}

tree :: (Ord a, Num a) => a -> Tree (a, a)
tree n = grow (f n) (1,1)

f :: (Num a, Ord a) => a -> (a, a) -> [(a, a)]
f n (x,y) = if x+y <= n then [(x+y,x),(x,x+y)] else []
