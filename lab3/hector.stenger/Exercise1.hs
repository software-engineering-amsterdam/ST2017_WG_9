module Exercise1 where

import Data.List
import Lecture3

{- Time: 1 hour -}

{- Satisfiable means there should exists a valuation that is True. -}
satisfiable :: Form -> Bool
satisfiable f = any (\v -> evl v f) (allVals f)

{- Contradiction means there shouldn't exist any valuation that is True. -}
contradiction :: Form -> Bool
contradiction f = all (\v -> not $ evl v f) (allVals f)

{- Tautology means there shouldn't exist any valuation that isn't True. -}
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

evalForm :: Form -> [Bool]
evalForm f = map (\v -> evl v f) (allVals f)

{- Equivalence means that all the evaluations from both forms should be -}
{- identical. -}
equiv :: Form -> Form -> Bool
equiv f g = bothAnd || leftEval == rightEval
    where leftEval = evalForm f
          rightEval = evalForm g
          bothAnd = (and leftEval) && (and rightEval)

{- Remove when done. -}
printer f = map (\v -> evl v f) (allVals f)

{- Entails means that the valuations from A should imply the valuations from B  -}
entails :: Form -> Form -> Bool
entails f g = (not $ equiv f g) && entailProperty (evalForm f) (evalForm g)

entailProperty :: [Bool] -> [Bool] -> Bool
entailProperty a b = and $ map (\(x,y) -> x --> y) (zip a b)

_p = Prop 1
_q = Prop 2
_r = Prop 3

{- (p -> q) -> (not q -> not p) -}
_form1 = Equiv (Impl _p _q) (Impl (Neg _q)(Neg _p))

{- (p -> q) -> (not p -> not q) -}
_form2 = Equiv (Impl _p _q) (Impl (Neg _p) (Neg _q))

{- ((p -> q) && (q -> r)) -> (p -> r) -}
_form3 = Impl (Cnj [Impl _p _q, Impl _q _r]) (Impl _p _r)

_form4 = Neg _p
_form5 = Cnj [Prop 1337, Prop 199]

{- p -> r -}
logicalSatisfiable = Impl _p _r

{- not p && p -}
logicalContradiction = Cnj [Neg _p, _p]

{- not p || p -}
logicalTautology = Dsj [Neg _p, _p]

{- A form should be equivalent to itself -}
equivCheck = equiv _form1 _form1

{- p -> q && (r || not r) -}
_form6 = Cnj [Impl _p _q, Dsj [_r, (Neg _r)]]

_form7 = Cnj [Dsj [Neg _q, _q], Dsj [Neg _p, _p]]

_equivalenceTests = [
    (_form1, _form1),
    (logicalTautology, _form7)]

_equivRunner = and $ map (\(f1,f2) -> equiv f1 f2) _equivalenceTests
