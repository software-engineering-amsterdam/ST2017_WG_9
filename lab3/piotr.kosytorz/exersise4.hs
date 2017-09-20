module Lab3Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import System.Random (randomRIO)

avialableProps = [p,q,r]

randomProp :: IO (Form)
randomProp = do
  pos <- randomRIO(0,length avialableProps - 1)
  return (avialableProps!!pos)

random1or0 :: IO Int
random1or0 = randomRIO (0,1)

randomNeg :: IO (Form) -> IO (Form)
randomNeg f = do
  r <- random1or0
  fn <- f
  if  r==1
  then
    return (Neg(fn))
  else
    return (fn)

random1to4 :: IO Int
random1to4 = randomRIO (1,4)

concatWithRandom2ArgOp :: (IO (Form), IO (Form)) -> IO (Form)
concatWithRandom2ArgOp (fm1,fm2) = do
  r <- random1to4 -- there are 3 2arg operators, so I'm choosing one of them randomly
  f1 <- fm1
  f2 <- fm2
  case r of
    1 -> return (Cnj [f1,f2])
    2 -> return (Dsj [f1,f2])
    3 -> return (Impl f1 f2)
    _ -> return (Equiv f1 f2)

randomForm :: Int -> IO (Form)
-- randomForm 0 = return []
randomForm n = do
  depth <- randomRIO (1,n)  -- depth of the subtree
  --y <- concatWithRandom2ArgOp (randomForm n-1, randomForm n-1)
  if
    depth == 1
  then do
    x <- randomNeg randomProp
    return (x)
  else do
    y <- randomNeg $ concatWithRandom2ArgOp (randomForm (n-1),randomForm (n-1))
    return (y)

{-- Logic for randomForm:
  -- if depth == 1 then leaf: raddom Prop + random Neg
  -- else
  -- random: leaf or node
    -- if leaf: raddom Prop + random Neg
    -- if node: random Op + random negation + recursion on operand1 and operand2
--}

{-- Testing --}

-- random form show wrapper
showRandomForm :: IO (Form) -> IO (String)
showRandomForm mf = do
  f <- mf
  return (show f)

-- random form parse wrapper
parseRandomForm :: IO (String) -> IO ([Form])
parseRandomForm ms = do
  s <- ms
  return (parse s)

-- form to IO Form wrapper
formToIOForm :: Form -> IO (Form)
formToIOForm f = do
  let x = f
  return x

-- test random form -> parameter is the depth of a tree
testRandomForm :: Int -> IO (Bool)
testRandomForm depth = do
  myRandomForm <- randomForm depth  -- Form
  parsedForm <- parseRandomForm ( showRandomForm $ formToIOForm myRandomForm ) -- [Form]
  return (parsedForm == [myRandomForm])
