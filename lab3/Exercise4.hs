module Exercise4 (testFormPredicate, testCnf, testParse) where

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
[?] sequence of test properties
[?] test report
[x] indication of time spent

Approach:
At first we tried to find diffrent solutions in our group. Instead of wildy
generating forms we thought about binary trees or AST trees. We found out that
the data types predefine a certain grammar (recursive data type) that could be
seen like a tree, which does not allow incorrect formulas.

Test the implementation:
The postcondition of the generator is that the outputted function is a valid
formula. Since the typechecking proofs that the output has to be of type IO Form
we can assume that every outputted form is valid (see grammar explanation above)
The only possibility to produce an error is to generate a form with zero
possible properties.

Using the generator to test formulas:
We can use the testFormPredicate like we used QuickCheck for numbers. In the
testing section we have tried out multiple predicated from prior exercises.

TODO:
- Write tests for exercise 1
-------------------------------------------------------------------------------}

import Lecture3 (Form (..), parse)
import Exercise1 (equiv, tautology)
import System.Random
import Exercise3 (cnf)

getRandomBoundedInt :: Int -> Int -> IO Int
getRandomBoundedInt l u = getStdRandom (randomR (l,u))

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
                            Tests
-------------------------------------------------------------------------------}

-- Taken from Exercise 2
_testParse :: Form -> Bool
_testParse f = f == head (parse $ show f)

testParse :: IO()
testParse = testFormPredicate 100 _testParse

testCnf :: IO()
testCnf = testFormPredicate 100 (\f -> equiv (cnf f) f)
