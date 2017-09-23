module Lab3ex1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Time spent: 1h 30 min

-- like in the lecture it is a contradiction if it is not satisfiable
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- it is a tautology when all possible outcomes result in true
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment
-- it is a logical entailment when all true values from f1 also have a true value in f2
entails :: Form -> Form -> Bool
entails f1 f2 = all (\ (b1,b2) -> b1-->b2) (zip r1 r2)
  where
    r1 = (map (\ v -> evl v f1) (allVals f1))
    r2 = (map (\ v -> evl v f2) (allVals f2))

-- | logical equivalence
-- it is logical equivalent when all results for both forms are the same
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\ (b1,b2) -> b1==b2) (zip r1 r2)
  where
    r1 = (map (\ v -> evl v f1) (allVals f1))
    r2 = (map (\ v -> evl v f2) (allVals f2))

contradictionForm ::Form
contradictionForm = Cnj[p,Neg p]

tautologyForm ::Form
tautologyForm = Dsj[p,Neg p]

{-

For testing i defined two forms

contradictionForm - which is a contradiction
tautologyForm - which is a tautology

output for contradiction and tautology functions:

*Lab3> contradiction contradictionForm
True
*Lab3> contradiction tautologyForm
False
*Lab3> tautology contradictionForm
False
*Lab3> tautology tautologyForm
True

-> our functions work as expected

output for equiv

*Lab3> equiv contradictionForm contradictionForm
True
*Lab3> equiv contradictionForm tautologyForm
False
*Lab3> equiv tautologyForm contradictionForm
False
*Lab3> equiv tautologyForm tautologyForm
True

-> only trues for when results are the same


output for entails

*Lab3> entails contradictionForm contradictionForm
True
*Lab3> entails contradictionForm tautologyForm
True
*Lab3> entails tautologyForm contradictionForm
False
*Lab3> entails tautologyForm tautologyForm
True

-> only trues for when true in f1 == true in f2

-}
