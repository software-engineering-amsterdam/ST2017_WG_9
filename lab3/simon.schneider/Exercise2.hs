module Exercise2 where

import Exercise1
import Lecture3

{--
Time spent: 45m (understanding parse function from lecture, implement test)

Since a form can easily be converted to a string we can test if a parsed
function fits a string representation.
You can even go so far and test if a parsed string representation of an existing
function is still the same as the original function (see selfTestParse)

Test conditions:
  function 1 := f1,
  string representation of f1 := s
  parsed string representation of f1 := f2

- s can be parsed into one valid form
- f1 is equal to f2 (same truth table, see Exercise 1)
- the string representation of f1 is equal to s and string representation of f2

--}

testParse :: Form -> String -> Bool
testParse f1 s =
    length (parse s) == 1
    && show f1 == s
    && show f1 == show f2
    && equiv f1 f2
  where f2 = head(parse s)

selfTestParse :: Form -> Bool
selfTestParse f = testParse f (show f)

test :: Bool
test = all (== True) [
    selfTestParse form1, selfTestParse form2, selfTestParse form3,
    testParse (Cnj [q, p]) "*(2 1)",
    testParse (Dsj [q, p]) "+(2 1)",
    not (testParse (Cnj [q, p]) "1 + 2")
  ]
