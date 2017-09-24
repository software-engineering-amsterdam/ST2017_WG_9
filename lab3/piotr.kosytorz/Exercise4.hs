module Exercise4 where

import Data.List
import System.Random
import Lecture3
import Exercise3

{--
  Time spent: 3h
--}

{--
  there must be a set of possible propositions, so I'm using here the 3: p,q,r
  that are given in Lecture3.hs
--}
avialableProps = [p,q,r]

-- picks a random proposition from the given set of avialableProps - (p, q or r)
randomProp :: IO (Form)
randomProp = do
  pos <- randomRIO(0,length avialableProps - 1) -- equal chances
  return (avialableProps!!pos)                  -- returns one element

-- generates 0 or 1 with (hopefully ;) equal (50%) chances for each
random1or0 :: IO Int
random1or0 = randomRIO (0,1)

-- randomly adds a negation to literal or formula
randomNeg :: IO (Form) -> IO (Form)
randomNeg f = do
  r <- random1or0
  fn <- f
  if  r==1
  then
    return (Neg(fn))
  else
    return (fn)

-- returns randomly a number between 1 and 4 (there are 4 binary operations)
random1to4 :: IO Int
random1to4 = randomRIO (1,4)

{--
  takes two formulas and creates a new one as a result of concatenating them
  with one of the 4 binary operations: Cnj, Dsj, Impl, Equiv

  Note!: Cnj and Dsj are binary in their nature. The fact that they accept
  lists of elements is purely dictated by the implementation gieven in
  Lecture3.hs
--}
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

{--
  Finally the final form generator
--}
randomForm :: Int -> IO (Form)
randomForm n = do
  depth <- randomRIO (1,n)  -- n = depth of the subtree
  if
    depth == 1
  then do
    x <- randomNeg randomProp -- if the tree has only 1 element left, then it's a leaf and a leaf nust be a literal
    return (x)
  else do
    y <- randomNeg $ concatWithRandom2ArgOp (randomForm (n-1),randomForm (n-1))
    {--
      If the depth is > 1, then concat 2 subrees with a random 2arg operand and set randomly a negation to it
    --}
    return (y)

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

{--
  Results:
  *Exercise4> testRandomForm 10
  True
--}

{-- Additional tests (testing Ex. 3) --}
{--
  The idea is to generate a list of random formulas, then generate cnf for those
  formulas and eventually compare the truth tables of the cnf formulas and the
  original ones.
--}
