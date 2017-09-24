module Exercise5Bonus where

import Lecture3
import Exercise1 (equiv)
import Exercise3 (cnf)
import Exercise4 (testFormPredicate)

type Clause  = [Int]
type Clauses = [Clause]

-- Form:            (1 OR 3) AND (-2 OR 3)
-- Expected output: [[1,3], [-2,3]]
cnfExample :: Form
cnfExample = Cnj [Dsj [p,r], Dsj [Neg q, r]]

literalToNumber :: Form -> Int
literalToNumber (Prop j) = j
literalToNumber (Neg (Prop j)) = j * (-1)
literalToNumber _ = 0

cnf2cls :: Form -> Clauses
cnf2cls (Cnj a) = map dsjToClause a
cnf2cls d@(Dsj _) = [dsjToClause d]
cnf2cls (Prop a) = [[a]]
cnf2cls (Neg (Prop j)) = [[j * (-1)]]
cnf2cls _ = []

dsjToClause :: Form -> Clause
dsjToClause (Dsj a) = map literalToNumber a
dsjToClause _ = []

numberToLiteral :: Int -> Form
numberToLiteral j
  | j >= 0 = Prop j
  | otherwise = Neg( Prop (j * (-1)))

cls2cnf :: Clauses -> Form
cls2cnf [] = Cnj []
cls2cnf [[f]] = numberToLiteral f
cls2cnf cs = Cnj dsjs
  where dsjs = map (\c -> Dsj ( map numberToLiteral c) ) cs

testExample :: Bool
testExample =  cls2cnf (cnf2cls cnfExample) == cnfExample

testConversions :: IO()
testConversions = testFormPredicate 10 (\f -> (cls2cnf (cnf2cls (cnf f)) `equiv` cnf f))
