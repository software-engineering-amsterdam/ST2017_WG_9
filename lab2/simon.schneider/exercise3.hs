{-
//TODO: Add description and time estimation
-}

module Exercise3 where

{------------------------------------------------------------------------------
                        Functions given by the exercise
-------------------------------------------------------------------------------}

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p


{------------------------------------------------------------------------------
                        Defining the conditions of Workshop 2
-------------------------------------------------------------------------------}
p11 :: Int -> Bool
p11 x = even x && x > 3

p12 :: Int -> Bool
p12 = even

p21 :: Int -> Bool
p21 x = even x || x > 3

p22 :: Int -> Bool
p22 = even

p31 :: Int -> Bool
p31 x = (even x && x > 3) || even x

p32 :: Int -> Bool
p32 = even

p41 :: Int -> Bool
p41 = even

p42 :: Int -> Bool
p42 x = (even x && x > 3) || even x


{------------------------------------------------------------------------------
                        Testing for weakness
-------------------------------------------------------------------------------}
p11Stronger :: Bool
p11Stronger = stronger [(-10)..10] p11 p12

{-//TODO: discuss with group what to test, make list by hand or automatically -}
