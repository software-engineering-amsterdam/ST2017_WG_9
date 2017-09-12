import Data.List

{-- Time spent: 15m --}

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

p1, p2, p3, p4, p5, p6, p7, p8 :: Integer -> Bool
p1 x = even x && x > 3
p2 x = even x

p3 x = even x || x > 3
p4 x = even x

p5 x = (even x && x > 3) || even x
p6 x = even x

p7 x = even x
p8 x = (even x && x > 3) || even x

{-- Testing --}

domain = [-10..10]

testP1strongerThanP2 = stronger domain p1 p2
testP2strongerThanP1 = stronger domain p2 p1

testP3strongerThanP4 = stronger domain p3 p4
testP4strongerThanP3 = stronger domain p4 p3

testP5strongerThanP6 = stronger domain p5 p6
testP6strongerThanP5 = stronger domain p6 p5

testP7strongerThanP8 = stronger domain p7 p8
testP8strongerThanP7 = stronger domain p8 p7

{-- TODO: a descending list of strengths --}
