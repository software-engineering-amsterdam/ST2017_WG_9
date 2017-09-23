module Lab3ex4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3ex1
import Lab3ex3

-- Time spent:
-- 4h on first try with QuickCheck and Strings
-- 1h on second try with quickcheck and tree + tests
--

{-
instance Arbitrary Form where
  arbitrary = sized genForm

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

-- 2nd try
-- Information on how to use QuickCheck Generator see
-- https://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html#1.0

-- set default generator for type Form
instance Arbitrary Form where
    arbitrary = genForm

-- default generator to generate Forms with
-- a max tree size of 10
genForm :: Gen Form
genForm =
      do
        size <- elements [1..10]
        f <- genFormS size
        return f

-- Tree building function
genFormS :: Int -> Gen Form
genFormS n =
          if n == 1
            then do
              p <- elements [p,q,r]
              rp <- randomNegation p
              return rp
          else do
            c <- elements [0..3]
            f1 <- genFormS (n-1)
            f2 <- genFormS (n-1)
            let f = case c of
                         0 -> Cnj[f1, f2]
                         1 -> Dsj[f1, f2]
                         2 -> Impl f1 f2
                         _ -> Equiv f1 f2
            nf <- randomNegation f
            return nf

randomNegation :: Form -> Gen Form
randomNegation f =
              do
                rn <- elements [0,1]
                if rn == 0
                  then return (Neg f)
                  else return f

-- The test function
-- We check whether the truth tables from input form and the cnf of input form
-- are equivalent

-- Exaple usage: quickCheck testCnf
-- Output: +++ OK, passed 100 tests.

testCnf :: Form -> Bool
testCnf f = equiv f (cnf f)
