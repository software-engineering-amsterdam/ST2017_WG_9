module Lab3ex2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3ex1

-- Time spent: 1h

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
