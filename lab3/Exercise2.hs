module Exercise2 where

import Data.List
import System.Random
import Lecture3

{--

  Time Spent: 1h on implementation and tests

  The simplest way to test the parse function would be to parse the test formulas
  and check if the result matches the original one.

  Data Form derives Eq, so they are comparable.

  We chose to go with the equality check down below because it already includes
  all the other checks:
    - same datatype
    - same length
    - valid formular (is parsable)
    - equality of the formular

  With the check below we basically test that our parse function can read
  input produced by the show function.

--}


test :: Form -> Bool
test f = f == head (parse $ show f) -- parse returns a one-element list of formulas

{--
  For testing the following formulas can be used:
--}

testFormula1 = form1
testFormula2 = form2
testFormula3 = form3
testFormula4 = Equiv (Neg p) (p)
testFormula5 = Equiv form1 form3
testFormula6 = Cnj [form1,form2]
testFormula7 = Dsj [form2, form3]
testFormula8 = Impl form1 form2

{--
  A collecting list of all formulas to be tested:
--}
allFormulas = [testFormula1,
  testFormula2,
  testFormula3,
  testFormula4,
  testFormula5,
  testFormula6,
  testFormula7,
  testFormula8]

detailedTest = map (\f -> test f) allFormulas
{-- Resutls:
  *Lab3Exercise2> detailedTest
  [True,True,True,True,True,True,True,True]
--}

yesNoTest = all (\f -> test f) allFormulas
{-- Results:
  *Lab3Exercise2> yesNoTest
  True
--}
