module Exercise3 where

import Lecture3Dirty

equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals f1)

{--
time spent: 1h, learn about CNF conversions on internet, understand application
of de morgan rule, find ways to remove redundancies

--> Used piotrs solution insted which is less efficient but implemented in a
very elegant way.

Task:
The lecture notes of this week discuss the conversion of Boolean formulas
(formulas of propositional logic) into CNF form. The lecture notes also give a
definition of a Haskell formulas of propositional logic, using lists for
conjunctions and disjunctions.
Your task is to write a Haskell program for converting formulas into CNF.

Specification
(See https://en.wikipedia.org/wiki/Conjunctive_normal_form)

1. Remove arrows (arrow free form)
2. Convert to negation normal form. (move negations inwards)
3. Apply demorgan to remove to move conjunctions to the outside
4. Remove redundancies (q ∧ -q = False, q ∨ -q = True ...)
5. Test if functions are still equivalent

--}

_form1, _form1Cnf :: Form
_form1 = Impl (Impl p q) r
_form1Cnf = Cnj [Dsj [p,r], Dsj [Neg q, r]]
_form2 = Neg (Dsj [p,r])
_form3 = Dsj [Cnj [p,Neg q], r]
_form4 = Dsj [Cnj [p, q], r]
_form5 = Dsj [Cnj [p, q], r]


_testForm1 :: Bool
_testForm1 = equiv _form1 _form1Cnf

cnfConvert :: Form -> Form
cnfConvert = cnfConvertBrackets . nnf . arrowfree

cnfConvertBrackets :: Form -> Form
cnfConvertBrackets (Dsj [f1,f2]) = demorgan f1 f2
cnfConvertBrackets f = f

{--
Apply demorgans law to bring nnf formula into cnf
P ∨ (Q ∧ R)) ⇔ (P ∨ Q) ∧ (P ∨ R)
P ∨ (Q ∧ R ∧ N)) ⇔ (P ∨ Q) ∧ (P ∨ R)  ∧ (P ∨ N)
P ∨ (Q ∧ (L ∨ R))) ⇔ (P ∨ Q) ∧ (P ∨ (L ∨ R))
--}
demorgan :: Form -> Form -> Form
demorgan _ (Cnj []) = Cnj []
demorgan (Cnj []) _ = Cnj []
demorgan f (Cnj [g]) = demorgan f g
demorgan (Cnj [f]) g = demorgan f g
demorgan f (Cnj (g:gs)) = Cnj [demorgan f g, demorgan f (Cnj gs)]
demorgan (Cnj (f:fs)) g = Cnj [demorgan f g, demorgan (Cnj fs) g]
demorgan f g = Dsj [f,g]

demorganTest1 = demorgan p (Cnj [q, r])
