module Exercise5Bonus where

import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

-- Form:            (1 OR 3) AND (-2 OR 3)
-- Expected output: [[1,3], [-2,3]]
cnfExample :: Form
cnfExample = Cnj [Dsj [p,r], Dsj [Neg q, r]]

propToNumber :: Form -> Int
propToNumber (Prop j) = j
propToNumber (Neg (Prop j)) = j * (-1)
propToNumber _ = error "Not a cnf"

cnf2cls :: Form -> Clauses
cnf2cls (Cnj a) = map dsjToClause a
cnf2cls _ = error "Not a cnf"

dsjToClause :: Form -> Clause
dsjToClause (Dsj a) = map propToNumber a
dsjToClause _ = error "Not a cnf"
