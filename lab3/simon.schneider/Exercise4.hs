module Exercise4 where

{------------------------------------------------------------------------------
time spent: 1h 30m (see ../Exercise4.hs)
-------------------------------------------------------------------------------}


import Lecture3
import Lecture2
import Exercise1 (equiv)
import System.Random
import Exercise3Piotr

getRandomBoundedInt :: Int -> Int -> IO Int
getRandomBoundedInt l u = getStdRandom (randomR (l,u))

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
                        Test
-------------------------------------------------------------------------------}

formsTester :: [Form] -> (Form -> Bool) -> IO ()
formsTester [] _ = putStrLn "All tests passed"
formsTester (f:fs) p = do
  formsTester fs p
  if p f then
    putStrLn "Test passed"
  else
    putStrLn ("Test failed: " ++ show f)

testFormPredicate :: Int -> (Form -> Bool) -> IO ()
testFormPredicate n p = do
  fs <- generateTestForms n
  (formsTester fs p)
