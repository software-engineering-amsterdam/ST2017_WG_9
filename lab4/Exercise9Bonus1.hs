module Exercise9Bonus1 where

import Data.List
import Data.String
import Lecture4

varA,varB :: Var
varA = "a"
varB = "b"

i9 :: Expr
i9 = I 9

instance Show Condition where
  show (Prp v) = v
  show (Eq e1 e2) = show e1 ++ " = " ++ show e2
  show (Lt e1 e2) = show e1 ++ " < " ++ show e2
  show (Gt e1 e2) = show e1 ++ " > " ++ show e2
  show (Cj xs) = intercalate " && " (map show xs)
  show (Dj xs) = intercalate " || " (map show xs)

instance Show Expr where
  show (I i) = show i
  show (V s) = s
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Subtr e1 e2) = show e1 ++ " - " ++ show e2
  show (Mult e1 e2) = show e1 ++ " * " ++ show e2

instance Show Statement where
  show (Ass v e)   = v ++ " = " ++ show e
  show (Cond c s1 s2)  = "if(" ++ show c ++  "){ \r\n"++ show s1 ++" \r\n}else{\r\n"++ show s2 ++"\r\n}"
  show (Seq ss) = intercalate "\r\n" (map (\s -> show s) ss)
  show (While c s) =  "while(" ++ show c ++  "){ \r\n"++ show s ++" \r\n}"

-- a = b + 9
statement1 = Ass varA (Add (V varB) (I 9))

{--
Before:
-------------------------------------------------------------------------------
Seq [Ass "x" (I 0),Ass "y" (I 1),While (Gt (V "n") (I 0)) (Seq [Ass "z" (V "x"),
Ass "x" (V "y"),Ass "y" (Add (V "z") (V "y")),Ass "n" (Subtr (V "n") (I 1))])]

After:
-------------------------------------------------------------------------------
x = 0
y = 1
while(n > 0){
z = x
x = y
y = z + y
n = n - 1
}

--}

fibCode :: String
fibCode = show fib

--TODO: Read and QuickCheck
