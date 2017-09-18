module Exercise3 where

import Lecture3
import Exercise1

{--
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

--}

_form1, _form1Cnf :: Form
_form1 = Impl (Impl p q) r
_form1Cnf = Cnj [Dsj [p,r], Dsj [Neg q, r]]
_form2 = Neg (Dsj [p,r])
_form3 = Dsj [Cnj [p,Neg q], r]
_form4 = Dsj [Cnj [p, q], r]


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
--}
demorgan :: Form -> Form -> Form
demorgan (Prop x) (Cnj [Prop y1, Prop y2]) = Cnj [Dsj [Prop y1, Prop x], Dsj [Prop y2, Prop x]]
demorgan (Cnj [Prop x1, Prop x2]) (Prop y) = Cnj [Dsj [Prop x1, Prop y], Dsj [Prop x2, Prop y]]
demorgan x1 x2 = Dsj [x1, x2]
