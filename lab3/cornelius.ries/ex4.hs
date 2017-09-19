module Lab3ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Time spent:

--instance Arbitrary Form where
--    arbitrary = sized genForm

{-
genFormS :: Gen String
genFormS =
   do op <- elements ["-", "*", "+", "==>", "<=>"]
      p <- shuffle ["0", "1", "2"]
      return (getResult op p)

genOp :: Gen String
genOp =
  do
    op <- elements ["*", "+", "==>", "<=>"]
    return op

genForm :: Int -> Gen Form
genForm size =
   do op <- vectorOf size genOp
      bla <- vectorOf size genFormS
      return (head(parse(getResult2 op bla)))

getResult2 :: [String] -> [String] -> String
getResult2 [] [] = ""
getResult2 _ [] = ""
getResult2 [] _ = ""
getResult2 (x:xs) (a:b:aa)
  | x == "*" = "*(" ++ a ++ " " ++ b ++ ")" ++ (getResult2 xs aa)
  | x == "+" = "+(" ++ a ++ " " ++ b ++ ")" ++ (getResult2 xs aa)
  | x == "==>" = "(" ++ a ++ "==>" ++ b ++ ")" ++ (getResult2 xs aa)
  | x == "<=>" = "(" ++ a ++ "<=>" ++ b ++ ")" ++ (getResult2 xs aa)
getResult2 (x:xs) (a:aa) = ""

getResult :: String -> [String] -> String
getResult op p
  | op == "-" = "-" ++ head p
  | op == "*" = "*(" ++ showLstS p ++ ")"
  | op == "+" = "+(" ++ showLstS p ++ ")"
  | op == "==>" = "(" ++ p !! 0 ++ "==>" ++ p !! 1 ++ ")"
  | op == "<=>" = "(" ++ p !! 0 ++ "<=>" ++ p !! 1 ++ ")"

showLstS, showRestS :: [String] -> String
showLstS [] = ""
showLstS (f:fs) = f ++ showRestS fs
showRestS [] = ""
showRestS (f:fs) = ' ': f ++ showRestS fs
-}
