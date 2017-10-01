module Exercise7 where


-- Time spent 2h

import Data.List
import Exercise2 (rtester', setPairTester, getRandomSet, getRandomIntBound, generateMany)
import Exercise5 (symClos)
import Exercise6 (trClos)
import SetOrd
import Test.QuickCheck

type Rel a = [(a,a)]

relation1 :: Rel Int
relation1 = [(1,2),(2,3),(3,4)]

{------------------------------------------------------------------------------
                        Properties of symmetric closure

Properties of symmetric closure: (Haskell road to logic p182)
- smallest relation S with all the properties
- Has all properties in O
- S has all properties in O, then R subset or equal toS
-------------------------------------------------------------------------------}

relationIsSubset :: Ord a => Rel a -> Rel a -> Bool
relationIsSubset r1 r2 = all (`elem` r2) r1

isSymmetric :: Ord a => Rel a -> Bool
isSymmetric r = all (\(a,b) -> (b,a) `elem` r) r

isMinimalSymmetric :: Ord a => Rel a -> Rel a -> Bool
isMinimalSymmetric r1 r2 = null newNonSymmetric
  where
    newPairs = r2 \\ r1
    newNonSymmetric = filter (\(a,b) -> (b,a) `notElem` r1) newPairs

testSymmetricClosure :: Ord a => Rel a -> Bool
testSymmetricClosure r1 = relationIsSubset r1 r2 && isSymmetric r2 && isMinimalSymmetric r1 r2
  where r2 = symClos r1

{------------------------------------------------------------------------------
                        Properties of transitive closure
- R+ is the sum of all compositions of a relation / "contains all
reachable paths" (informal)
- R+ is minimal
- R is subset of R+
-------------------------------------------------------------------------------}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

isTransitive :: Ord a => Rel a -> Bool
isTransitive r = all (`elem` r) (r @@ r)

isMinimalTranstive :: Ord a => Rel a -> Rel a -> Bool
isMinimalTranstive r1 r2 = null newNonTranstive
  where
    newPairs = r2 \\ r1
    newNonTranstive = filter (\p -> isTransitive (delete p r2)) newPairs

testTranstivieClosure :: Ord a => Rel a -> Bool
testTranstivieClosure r1 = relationIsSubset r1 r2 && isTransitive r2 && isMinimalTranstive r1 r2
  where r2 = trClos r1

{------------------------------------------------------------------------------
                          Relation tester from scratch
-------------------------------------------------------------------------------}


getRandomRelation' :: Int -> IO (Rel Int)
getRandomRelation' 0 = return []
getRandomRelation' n = do
  ps <- getRandomRelation' (n-1)
  l <- getRandomIntBound 1 7
  r <- getRandomIntBound 1 7
  return (sort(nub ((l,r) : ps)))

getRandomRelation :: IO (Rel Int)
getRandomRelation = do
  n <- getRandomIntBound 0 20
  getRandomRelation' n

relationTester :: Int -> (Rel Int -> Bool) -> IO()
relationTester n p = rtester' (generateMany n getRandomRelation) p

runSymmetricClosureTest :: IO()
runSymmetricClosureTest = relationTester 100 testSymmetricClosure

runTransitiveClosureTest :: IO()
runTransitiveClosureTest = relationTester 100 testTranstivieClosure


{------------------------------------------------------------------------------
                        How could QuickCheck be used?

Use positive int to generate random lists of random pairs, relPInt2RelInt then
converts them to inteter relations.
Calculating the transtive closure takes long for really long lists, as you
can see in run1 and run2, but they will both eventually terminate.
-------------------------------------------------------------------------------}
processIntRelation :: Rel Int -> Rel Int
processIntRelation r = sort (nub r)

qcSymmetricClosure :: Rel Int -> Bool
qcSymmetricClosure r = testSymmetricClosure (processIntRelation r)

qcTransitiveClosure :: Rel Int -> Bool
qcTransitiveClosure r = testTranstivieClosure (processIntRelation r)

run1, run2 :: IO()
run1 = verboseCheck qcSymmetricClosure
run2 = verboseCheck qcTransitiveClosure
