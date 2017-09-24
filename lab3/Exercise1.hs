module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{--
  Time spent: 2h
--}

contradiction :: Form -> Bool
contradiction formX = all (\x -> not $ evl x formX) $ allVals formX
{--
  contradiction is false only if all evaluations are false
  it is equal to
  contradiction = not . satisfiable ()
--}

tautology :: Form -> Bool
tautology formX = all (\x -> evl x formX) $ allVals formX
{--
  tautology is true only if all evaluations are true
--}

-- | logical entailment
entails :: Form -> Form -> Bool
entails formX formY = tautology $ Impl formX formY
{--
  B logically entails A is true if and only if it is necessary that if all of
  the elements of B are true, then A is true

  In other words :
  B entails A when all True evaluations of B are also True in A
--}

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv formX formY = tautology $ Equiv formX formY
{--
  Forms are equivalent, when their equivalence in propositional logic is a
  tautology.

  Alternative solution:
    equiv f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals f1)
  Problem:
    This works for formulas that have the same set of properties.
    The problem is that it wont work for two formulars that are equivalant but
    have a diffrent set of properties, i.e. f1: (p), f2: (p ∧ (q ∨ ¬q))
--}

-- | testing
{--
  A contradicting sentence:
  (p ∧ q) ∧ (¬p ∨ ¬q) ≡
  (p ∧ q) ∧ ¬(p ∧ q) ≡
  r ∧ ¬r = ⊥
--}
contractingStentence = Cnj[Cnj[p,q], Neg (Cnj[p,q])]

{--
  A tautology:
  (¬p ∨ p) ∧ (¬q ∨ q)
--}
tautologySentence = Cnj[Dsj[Neg p,p],Dsj[Neg q,q]]

{--
  An entailment:
  Sentence A: (p → q) ∧ (q → r)
  Sentence B: p → r
--}
entailmentSentenceA = Cnj[Impl p q, Impl q r]
entailmentSentenceB = Impl p r

{--
  Equivalent sentences (last two lines from proof contradicting senstences - here above):
  Sentence A: (p ∧ q) ∧ (¬p ∨ ¬q)
  Sentence B: (p ∧ q) ∧ ¬(p ∧ q)
--}
equivalantSentenceA = Cnj[Cnj[p,q],Dsj[Neg p, Neg q]]
equivalantSentenceB = Cnj[Cnj[p,q],Neg (Cnj[p,q])]

-- test contradiction
test1 = contradiction contractingStentence
test1a = not $ tautology contractingStentence

-- test tautology
test2 = tautology tautologySentence
test2a = not $ contradiction tautologySentence

-- test entailment
test3 = entails entailmentSentenceA entailmentSentenceB
test3a = not $ entails entailmentSentenceA $ Neg entailmentSentenceB

-- test equivalence
test4 = equiv equivalantSentenceA equivalantSentenceB
test4a = not $ equiv equivalantSentenceA $ Neg equivalantSentenceB

{--
  Test results:

  *Lab3Exercise1> test1
  True
  *Lab3Exercise1> test1a
  True
  *Lab3Exercise1> test2
  True
  *Lab3Exercise1> test2a
  True
  *Lab3Exercise1> test3
  True
  *Lab3Exercise1> test3a
  True
  *Lab3Exercise1> test4
  True
  *Lab3Exercise1> test4a
  True
--}
