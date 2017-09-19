module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{--
TODO: Talk about super complex solution, compare

--}

tautology :: Form -> Bool
tautology formx = all (\x -> evl x formx) $ allVals formx

-- extracts valuations which give False as result
extractFalseResults :: Form -> [Valuation]
extractFalseResults f = filter (\x -> not $ evl x f) $ allVals f

-- Valuations to disjunction of literals
v2dl :: Valuation -> Form
v2dl (x:xs)
  | xs == [] = if snd x then Neg (Prop (fst x)) else Prop (fst x)
  | otherwise = if snd x then Dsj [Neg (Prop (fst x)), v2dl xs] else Dsj [Prop (fst x), v2dl xs]

-- Conjunction of formulas
cf :: [Form] -> Form
cf (x:xs)
  | xs == [] = x
  | otherwise = Cnj [x, cf xs]

-- CNF from
cnf :: Form -> Form
cnf f
  | tautology f = error "Given formula is a tautology." --TODO: Return a tautology for tests
  | otherwise = cf $ map (\x -> v2dl x) $ extractFalseResults f
