module Exercise8 where

import Data.List
import Exercise2 (generateMany)
import Exercise5 (symClos)
import Exercise6 (trClos)
import Exercise7

{--

Time 30m, thinking about the problem + implement solution

Task:
Is there a difference between the symmetric closure of the transitive closure of
a relation RR and the transitive closure of the symmetric closure of RR?

--> Find counterexamples and examples

--}

generateRelations :: Int -> IO [Rel Int]
generateRelations n = generateMany n getRandomRelation

areSame :: Rel Int -> Bool
areSame r = trClos (symClos r) == symClos (trClos r)

findCounterExamples :: IO [Rel Int]
findCounterExamples = do
  rs <- generateRelations 100
  return (nub (filter (not . areSame) rs))


findExamples :: IO [Rel Int]
findExamples = do
  rs <- generateRelations 100
  return (nub (filter areSame rs))

{------------------------------------------------------------------------------
                    Answer:

*Exercise8> findExamples
[[(1,1),(2,4),(3,5),(4,2),(5,1),(5,3),(7,7)],[],[(2,3),(3,2),(3,6),(3,7),(4,2),
(4,5),(5,4),(5,7),(6,3),(7,5),(7,7)],[(1,3),(2,7),(3,4),(3,5),(3,7),(4,6)...

*Exercise8> findCounterExamples
[[(1,2),(1,3),(1,7),(2,2),(4,4),(5,1),(6,3),(6,5),(6,7),(7,3),(7,5)],[(6,4),
(6,7)],[(1,2),(2,5),(3,1),(3,3),(3,4),(3,6),(4,1),(6,3),(6,6)...

We can easily use Haskell to disprove that they are the same (by finding
counter examples)

Explanation on easy counter example: [(1,5)]
1. [(1,5)] -> sym -> [(1,5),(5,1)] -> trans -> [(1,5),(5,1),(5,5),(1,1)]
2. [(1,5)] -> trans -> [(1,5)] -> sym -> [(1,5),(5,1)]
-------------------------------------------------------------------------------}
