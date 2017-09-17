import Data.List
import Test.QuickCheck

{- Time: 30 minutes -}
infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker xs p q = stronger xs q p

p0,p1,p2,p3 :: Int -> Bool
p0 x = even x
p1 x = even x && x > 3
p2 x = (even x || x > 3) || even x
p3 x = p1 x || even x

domain = [(-10)..10]
first = stronger domain p1 p0
second = stronger domain p2 p0
third = stronger domain p3 p0
fourth = stronger domain p0 p3
