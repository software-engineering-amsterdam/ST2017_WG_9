module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{--
  Time spent: 2h
--}

{--
  There are multiple ways to generate a CNF form from a propositional logic formula.
  The most obvious and direct one would be to implement the standard algorithm that
  involves usage og the double negative law, De Morgan's laws, and the distributive law.


  Another way is the method that we've used in the workshop and that is based on
  retrieving a CNF form from furmula's truth table. I've decided to implement this
  one because it is correct and easy to follow. It has a big disadvantage though:
  its memory complexicity. Nevertheless is still a correct method and easy to
  implement.

--}

-- | a test for tautologies
tautology :: Form -> Bool
tautology formx = all (\x -> evl x formx) $ allVals formx

-- | extracts valuations which give False as result (comp. https://math.stackexchange.com/questions/636119/find-dnf-and-cnf-of-an-expression#637445)
extractFalseResults :: Form -> [Valuation]
extractFalseResults f = filter (\x -> not $ evl x f) $ allVals f

-- | valuations to disjunction of literals
{--
  The first step is to construct the disjuntive sub-lists based on values from
  the truth table (the rows),
  So each row that evaluates a False result is taken and with v2dl the single
  propositions are negated whenever they have a True value, next all obtained
  literals are set in a disjuntion.
--}
v2dl :: Valuation -> Form
v2dl (x:xs)
  | xs == [] = if snd x then Neg (Prop (fst x)) else Prop (fst x) -- the last element on the list doesn't have to be set in a Dsj list, we'll take care f that earlier
  | otherwise = if snd x then Dsj [Neg (Prop (fst x)), v2dl xs] else Dsj [Prop (fst x), v2dl xs] -- not reached the last element yet, which means that the literals have to be created and the Dsj list has to be expanded

-- Conjunction of formulas
{--
  The last step is to put everything togerher, so to make a Conjunction of all rows:
--}
cf :: [Form] -> Form
cf (x:xs)
  | xs == [] = x
  | otherwise = Cnj [x, cf xs]

-- CNF from
{--
  Finaly the CNF form puts all the helpers together in one set of operations,
  but fist checks if the formula is a tautology.
  We've agreed to use p ∨ ¬p as the basic formula for tautology.
  The Form data structure given in Lecture3.hs doesn't provide True and False
  as possible elements of the language.
--}
cnf :: Form -> Form
cnf f
  | tautology f = Dsj [p, Neg p]
  | otherwise = cf $ map (\x -> v2dl x) $ extractFalseResults f
