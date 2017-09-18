import Data.List

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
{-- Results:
  *Main> testP1strongerThanP2
  True
  *Main> testP2strongerThanP1
  False
--}

testP3strongerThanP4 = stronger domain p3 p4
testP4strongerThanP3 = stronger domain p4 p3
{-- Results:
  *Main> testP3strongerThanP4
  False
  *Main> testP4strongerThanP3
  True
--}

testP5strongerThanP6 = stronger domain p5 p6
testP6strongerThanP5 = stronger domain p6 p5
{-- Results:
  *Main> testP5strongerThanP6
  True
  *Main> testP6strongerThanP5
  True
--}

testP7strongerThanP8 = stronger domain p7 p8
testP8strongerThanP7 = stronger domain p8 p7
{-- Results:
  *Main> testP7strongerThanP8
  True
  *Main> testP8strongerThanP7
  True
--}

{-- Function "compar" given on the lecture: --}
compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

{-- to compare the properties we first have to extract the unique ones: --}

unique1 = even
unique2 x = even x && x > 3
unique3 x = even x || x > 3
unique4 x = (even x && x > 3) || even x

{-- unique1 and unique4 are the same, so I will get rid of unique4 --}
{-- There are 3 pairs:

  unique1 - unique2
  unique2 - unique3
  unique3 - unique1

  The results are:

  *Main> compar domain unique1 unique2
  "weaker"
  *Main> compar domain unique2 unique3
  "stronger"
  *Main> compar domain unique3 unique1
  "weaker"
  *Main>

  so:

  u1 < u2
  u3 < u2
  u3 < u1

  the result is:

  u3 < u1 < u2

--}
