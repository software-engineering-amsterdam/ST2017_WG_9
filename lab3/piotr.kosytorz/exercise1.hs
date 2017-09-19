module Lab3Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction formx = all (\x -> not $ evl x formx) $ allVals formx
{--
  contradiction is false only if all evaluations are false
  it is equal to
  contradiction = not . satisfiable
--}

tautology :: Form -> Bool
tautology formx = all (\x -> evl x formx) $ allVals formx
{--
  tautology is true only if all evaluations are true
--}

-- | logical entailment
entails :: Form -> Form -> Bool
entails formx formy = tautology $ Impl formx formy
{--
  B logically entails A is true if and only if it is necessary that if all of
  the elements of B are true, then A is true

  In other words :
  B entails A when all True evaluations of B are also True in A 
--}

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv formx formy = tautology $ Equiv formx formy
{--
  Forms are equivalent, when their equivalence in propositional logic is a
  tautology.
--}
