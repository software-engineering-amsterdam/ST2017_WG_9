module Lab3ex2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Time spent: 1h

-- | logical equivalence
-- it is logical equivalent when all results for both forms are the same
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\ (b1,b2) -> b1==b2) (zip r1 r2)
  where
    r1 = (map (\ v -> evl v f1) (allVals f1))
    r2 = (map (\ v -> evl v f2) (allVals f2))

testParse :: Form -> Bool
testParse f = equiv f parsed
  where
    parsed = head(parse(show f))

{-

*Lab3ex2> testParse form1
True
*Lab3ex2> testParse form2
True
*Lab3ex2> testParse form3
True


-}
