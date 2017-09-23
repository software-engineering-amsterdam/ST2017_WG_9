module Lab3ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3ex1

-- Time spent: 6h
-- 5h trying to apply the solution from the lecture
-- 1h rewriting cnf with the truth table approach

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


cnft :: Form -> Form
cnft = nnf . arrowfree

findNotEvals :: Form -> [Valuation]
findNotEvals f = filter (\x -> not(evl x f)) (allVals f)

buildDsj :: Valuation -> Form
buildDsj l = Dsj(subForms)
  where subForms = map (\x -> if snd x then Neg(Prop (fst x)) else Prop (fst x)) l

buildCnf :: [Valuation] -> Form
buildCnf l = Cnj( map (\x -> buildDsj x) l )

cnf :: Form -> Form
cnf f = buildCnf (findNotEvals f)
