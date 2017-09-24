module Exercise4 (testFormPredicate, testCnf, testParse, testTautology, testContradiction, testEquiv1, testEquiv2) where

{------------------------------------------------------------------------------
Time spent: 2h (finding a good approach, figuring monads out (again), implement
, write tests)

Task:
Write a formula generator for random testing of properties of propositional
logic, or teach yourself enough QuickCheck to use random QuickCheck testing of
formulas. Use your random testing method to test the correctness of the
conversion program from the previous exercise. Formulate a number of relevant
properties to test, and carry out the tests, either with your own random formula
generator or with QuickCheck.
Deliverables:
[x] generator for formulas
[x] sequence of test properties
[x] test report
[x] indication of time spent

Approach:
At first we tried to find diffrent solutions in our group. Instead of wildy
generating forms we thought about binary trees or AST trees. We found out that
the data types predefine a certain grammar (recursive data type) that could be
seen like a tree, which does not allow incorrect formulas.

Hector used a binary tree as the basis for his approach. It worked out by
splitting a sub formular like *(1 2 3) into *(1 *(2 *(3))). To achieve this
he had to change data definitions in the Lecture3.hs - this is the main reason
why we have chosen this solution

Test the implementation:
The postcondition of the generator is that the outputted function is a valid
formula. Since the typechecking proofs that the output has to be of type IO Form
we can assume that every outputted form is valid (see grammar explanation above)
The only possibility to produce an error is to generate a form with zero
possible properties, which the `generateTestForms` function does not allow.

Using the generator to test formulas:
We can use the testFormPredicate like we used QuickCheck for numbers. In the
testing section we have tried out multiple predicated from prior exercises.

Example test report:
testParse
Execute 100 tests
Test passed
Test passed
...
Test passed
All tests passed

Example Fail: (Test runner shows which tests fails and stops)
*Exercise4> testFormPredicate 10 (\f -> f `equiv` Neg f)
Execute 10 tests
Test failed: *(*(-4 -4 (3<=>5) +(2 2 3 4 3 1 5) *(3 1 2 5 5 2 4 3 1 1) -3
(2==>3) (4<=>5) -2) +((3<=>3) 4 (4==>3) *(2 5 5 1 1 1 5)) +(-5 +(4 2 4 5 5 5 1 5)
 3 (3<=>2) +(2 5 3 2 1 4) +(1 5 2 1 1 2 5 4 4 5) (1<=>4) -4 +(3 3 3 5 3 5 5))
 (2<=>-4) -(3<=>4) +(*(5 4 3 1 4) 3) -1 ((3<=>4)==>-3) --2)

-------------------------------------------------------------------------------}

import Lecture3 (Form (..), parse, q, p, r)
import Exercise1 (equiv, tautology, contradiction)
import System.Random
import Exercise3 (cnf)

getRandomBoundedInt :: Int -> Int -> IO Int
getRandomBoundedInt l u = getStdRandom (randomR (l,u))

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

{------------------------------------------------------------------------------
                        Implementation
-------------------------------------------------------------------------------}

getNodeTypeName :: Int -> String
getNodeTypeName 0 = "Prop"
getNodeTypeName 1 = "Neg"
getNodeTypeName 2 = "Or"
getNodeTypeName 3 = "And"
getNodeTypeName 4 = "Impl"
getNodeTypeName 5 = "Equiv"
getNodeTypeName _ = error "Cannot get node type name"

getRandomNodeType :: IO String
getRandomNodeType = do
  r <- getRandomBoundedInt 0 5
  return (getNodeTypeName r)

generateForm :: Int -> Int -> IO Form
generateForm 0 _ = do
  v <- getRandomBoundedInt 1 5
  return (Prop v)
generateForm _ 0 = error "Cannot generate form 0 atoms"
generateForm d n = do
  o <- getRandomNodeType
  v <- getRandomBoundedInt 1 5
  r2 <- getRandomBoundedInt 2 10
  f2 <- generateForm (d - 1) n
  f3 <- generateForm (d - 1) n
  ff <- generateForms r2 (d-1) n
  case o of
    "Prop" -> return (Prop v)
    "Neg" -> return (Neg f2)
    "Or" -> return (Dsj ff)
    "And" -> return (Cnj ff)
    "Impl" -> return (Impl f2 f3)
    "Equiv" -> return (Equiv f2 f3)
    _ -> error "Cannot generate form for unkown operator"

generateForms :: Int -> Int -> Int -> IO [Form]
generateForms 0 _ _ = return []
generateForms j d n = do
  h <- generateForm d n
  t <- generateForms (j-1) d n
  return (h:t)

generateTestForms :: Int -> IO [Form]
generateTestForms 0 = return []
generateTestForms j = do
  d <- getRandomBoundedInt 1 5
  n <- getRandomBoundedInt 1 10
  h <- generateForm d n
  t <- generateTestForms (j - 1)
  return (h:t)

{------------------------------------------------------------------------------
                        Testing suite
-------------------------------------------------------------------------------}

formsTester :: [Form] -> (Form -> Bool) -> IO ()
formsTester [] _ = putStrLn "All tests passed"
formsTester (f:fs) p =
  if p f then
    do
      putStrLn "Test passed"
      formsTester fs p
  else
    putStrLn ("Test failed: " ++ show f)

testFormPredicate :: Int -> (Form -> Bool) -> IO ()
testFormPredicate n p = do
  putStrLn ("Execute " ++ show n ++ " tests")
  fs <- generateTestForms n
  formsTester fs p


{------------------------------------------------------------------------------
                            Using generator to test
-------------------------------------------------------------------------------}

-- Taken from Exercise 2
_testParse :: Form -> Bool
_testParse f = f == head (parse $ show f)

testParse :: IO()
testParse = testFormPredicate 100 _testParse

testCnf :: IO()
testCnf = testFormPredicate 100 (\f -> equiv (cnf f) f)

testTautology :: IO()
testTautology = testFormPredicate 100 (\f -> tautology f --> f `equiv` Dsj [q, Neg q])

testContradiction :: IO()
testContradiction = testFormPredicate 100 (\f -> contradiction f --> f `equiv` Cnj [q, Neg q])

testEquiv1 :: IO()
testEquiv1 = testFormPredicate 100 (\f -> f `equiv` f)

testEquiv2 :: IO()
testEquiv2 = testFormPredicate 100 (\f -> not (f `equiv` Neg f))
