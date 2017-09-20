module Lab3ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Time spent: 5h trying to apply the solution from the lecture

equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\ (b1,b2) -> b1==b2) (zip r1 r2)
  where
    r1 = (map (\ v -> evl v f1) (allVals f1))
    r2 = (map (\ v -> evl v f2) (allVals f2))

rdcnj :: Form -> Form
rdcnj (Prop x) = Prop x
rdcnj (Neg f) = Neg (rdcnj f)
rdcnj (Dsj fs) = Dsj (map rdcnj fs)
rdcnj (Cnj (x:y:xs))
  | equiv x y && null xs = y
  | equiv x y = Cnj (map rdcnj (y:xs))
  | otherwise = Cnj (x:map rdcnj (y:xs))

rddsj :: Form -> Form
rddsj (Prop x) = Prop x
rddsj (Neg f) = Neg (rddsj f)
rddsj (Cnj fs) = Cnj (map rddsj fs)
rddsj (Dsj (x:y:xs))
    | equiv x y && null xs = y
    | equiv x y = Dsj (map rddsj (y:xs))
    | otherwise = Dsj (x:map rddsj (y:xs))


cnf :: Form -> Form
cnf = nnf . arrowfree

testform1 = Cnj[p, p, q]
