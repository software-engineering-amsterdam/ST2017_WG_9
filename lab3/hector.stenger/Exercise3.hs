module Exercise3 where

import Exercise1
import Lecture3

{- Time: 4.5 hour -}
{- First I tried rewriting the Props using demorgan and distributive but couldn't get my head around. -}
{- Got inspired by: https://math.stackexchange.com/questions/214338/how-to-convert-to-conjunctive-normal-form -}
{- . Ended up using the solution from Piotr. Change a couple of guards to pattern matching as I find -}
{- it more appealing and readable. -}

_CnjForm = Cnj [Prop 1337, Prop 199]
_ImplForm = Impl (Prop 1337) (Prop 199)

{- (p and q) or r === (p or r) and (q or r) -}
_MorganForm = Dsj [Cnj [Prop 1, Prop 2], Prop 3]

{- r or (p and q) === (r or p) and (r or q) -}
_MorganForm2 = Dsj [Prop 3, Cnj [Prop 1, Prop 2]]

{- 3 or 4 or 5 or (6 and 7) === (3 or 4 or 5 or 6) and (3 or 4 or 5 or 7) -}
_MorganForm3 = Dsj [Prop 3, Prop 4, Prop 5, Cnj [Prop 1, Prop 2]]

{- cnf2 :: Form -> Form -}
{- cnf2 f = morgan . nnf . arrowfree $ f -}

{- morgan :: Form -> Form -}
{- morgan (Dsj (x:xs)) = Cnj (concat $ map (\(Cnj y) -> map (\z -> Dsj [x, z]) y) xs) -}
{- morgan (Dsj fs) -}
    {- | hasConjuction fs =  -}
    {- | otherwise = fs -}
{- morgan f = f -}

{- Only return the valuations wich evaluate to false. -}
falseEvaluations :: Form -> [Valuation]
falseEvaluations f = filter (\x -> not $ evl x f) $ allVals f

v2dl :: Valuation -> Form
v2dl (x:[])
    | snd x = Neg (Prop (fst x))
    | otherwise = Prop (fst x)
v2dl (x:xs)
    | snd x = Dsj [Neg (Prop (fst x)), v2dl xs]
    | otherwise = Dsj [Prop (fst x), v2dl xs]

cf :: [Form] -> Form
cf (x:[]) = x
cf (x:xs) = Cnj [x, cf xs]

cnf :: Form -> Form
cnf f
  | tautology f = Cnj[]
  | otherwise = cf $ map (\x -> v2dl x) $ falseEvaluations f
