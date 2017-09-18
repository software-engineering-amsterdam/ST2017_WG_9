module Exercise1 (tautology, equiv) where

import Lecture3

{--
Time spent: 1h (learn about logic entailment, find test cases, implement)

After searching for multiple formulas that I can use for testing I implemented
the functions.
I then tested the implementation against the formulas and checked if a counter-
example will yield false (e.g. tautology form1 = False and not True)


--------------------------------------------------------------------------------
                        Test cases (example forms)
-------------------------------------------------------------------------------}

_formTautology, _formContradiction, _form1, _form2, _form11 :: Form
_formTautology = Dsj [p, Neg p]
_formContradiction = Cnj [p, Neg p]

_form1 = Dsj [p, q]
_form11 = Neg (Cnj [Neg p , Neg q])
_form2 = Cnj [p, q]

{------------------------------------------------------------------------------
                        Implementation
-------------------------------------------------------------------------------}

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)

-- If f1 is true then f2 also has to be true
entails :: Form -> Form -> Bool
entails f1 f2 = all (entailsForValuation f1 f2) (allVals f1)

entailsForValuation :: Form -> Form -> Valuation -> Bool
entailsForValuation f1 f2 v = (r1 && r2) || not r1
  where
    r1 = evl v f1
    r2 = evl v f2


equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals f1)

{------------------------------------------------------------------------------
                        Test
-------------------------------------------------------------------------------

Test entails for _form1 and _form2

form1
#  a	b	(a ∨ b)
1  F	F	   F
2  F	T	   T
3  T	F	   T
4  T	T	   T

form2
# a	b	(a ∧ b)
1 F	F	   F
2 F	T	   F
3 T	F	   F
4 T	T	   T

Test:
*Exercise1> entails _form1 _form2
False => We can find a counterexample in Line #2 and #3
*Exercise1> entails _form2 _form1
True => form2 is only True in Line #4, form1 entails form2 because it is also
True in line #4
--}


{--
form11

# a	b	¬(¬a ∧ ¬b)
1 F	F	   F
2 F	T	   T
3 T	F	   T
4 T	T	   T

Test:
*Exercise1> equiv _form1 _form11
True --> Like we can see in the truth tables for _form1 and _form11 they are
equal
--}

testTautology, testContradiction, testEquiv, testEquiv2, testEntails, testEntails2, testTautology2, testContradiction2 :: Bool
testTautology = tautology _formTautology
testTautology2 = not (tautology _form1)
testContradiction = contradiction _formContradiction
testContradiction2 = not (contradiction _form1)
testEquiv = equiv _form1 _form11
testEquiv2 = not (equiv _form1 _form2)
testEntails = not (entails _form1 _form2)
testEntails2 = entails _form2 _form1

allTests :: [Bool]
allTests = [
  testTautology,
  testContradiction,
  testEquiv,
  testEquiv2,
  testEntails,
  testEntails2,
  testTautology2,
  testContradiction2]

test :: Bool
test = all (== True) allTests
