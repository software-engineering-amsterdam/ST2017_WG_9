module Workshop6 where
import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

-- Question 1
leafCount :: Blt a -> Int
leafCount (Leaf a) = 1
leafCount (Node a b) = leafCount a + leafCount b

-- Question 2
mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf a) = Leaf(f a)
mapB f (Node a b) = Node (mapB f a) (mapB f b)

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

-- Question 3
count :: Tree a -> Int
count (T _ []) = 1
count (T _ (x:xs)) = 1 + count x + sum(map count xs)


-- Question 4
depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1

-- Question 5
mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x l) = T x (map f l)
