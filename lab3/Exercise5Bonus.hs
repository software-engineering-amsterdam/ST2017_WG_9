module Exercise5Bonus where

import Data.List
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

cnfExample2 :: Form
cnfExample2 = head(parse "*(+(-1 +(-3 -4)) +(-1 +(3 4)) +(1 +(-3 4) +(1 +(3 -4))))")

{--

p1: +(-1 +(-3 -4)) = +(-1 -3 -4)
p2: +(-1 +(3 4)) = +(-1 3 4)
p3: +(1 +(-3 4) +(1 +(3 -4))) = +(1 -3 4 1 3 -4)
--}

testForm1 = head(parse "+(-1 +(-3 -4)")
testForm11 = head(parse "+(-1 -3 -4)")

unpackCnf :: Form -> Form
unpackCnf (Cnj xs) = Cnj (map (\f -> Dsj (nub (getProps f)))  xs)
unpackCnf f = Cnj []

getProps :: Form -> [Form]
getProps (Neg n) = [Neg n]
getProps (Prop j) = [Prop j]
getProps (Dsj (x:xs)) = getProps x ++ getProps (Dsj xs)
getProps (Cnj (x:xs)) = getProps x ++ getProps (Cnj xs)
getProps _  = []

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

toCnf :: Form -> Form
toCnf = unpackCnf . cnf

testConversions :: IO()
testConversions = testFormPredicate 50 (\f -> (cls2cnf (cnf2cls (toCnf f)) `equiv` toCnf f))
