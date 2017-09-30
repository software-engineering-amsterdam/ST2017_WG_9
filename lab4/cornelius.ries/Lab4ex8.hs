module Lab4ex8 where

import Data.List
import System.Random
import Test.QuickCheck
import Lab4ex5
import Lab4ex6

-- Time Spent: 15m
--
testProperty :: Rel Int -> Bool
testProperty a = symClos(trClos a) == trClos(symClos a)

testPropertyQ :: Rel (Positive Int) -> Bool
testPropertyQ a = symClos(trClos a) == trClos(symClos a)

-- properties above are to check wether the
--
-- symmetric closure of the transitive closure of a relation R and
-- the transitive closure of the symmetric closure of R
--
-- are the same or different

-- QuickCheck produces errors

-- testing the property with
-- testProperty testRel
-- gives false
--
-- so there is a difference between them or my implementaion is wrong ;)
-- after checking the outputs for testRel

-- symClos(trClos testRel)
-- [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]
--
-- trClos(symClos testRel)
-- [(1,2),(1,1),(1,3),(2,1),(2,3),(2,2),(2,4),(3,2),(3,4),(3,3),(4,3)]

-- it is clear that the test above is falsy because the lists are unordered
-- but comparing them manually they are still different
-- the test above could be rewritten with elem checks
