module Workshop6 where
import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
  (Leaf "Turing, Alan"))
  (Leaf "Goedel, Kurt")

-- leafCount :: Blt a -> Int
