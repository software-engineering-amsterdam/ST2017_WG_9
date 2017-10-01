import System.Random
import Test.QuickCheck
import Data.List
import Exercise2
import SetOrd

{- Time spend: 2 hours -}

{- Got inspired by https://stackoverflow.com/questions/27471710/checking-if-2-list-have-any-equal-element-haskell -}
{- to use the function intersect -}
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set x) (Set y) = list2set $ intersect x y

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set x) (Set y) = list2set $ x ++ y

{- Thanks to Piotr's solution I discovered Data.List had this operator -}
setDiff :: Ord a => Set a -> Set a -> Set a
setDiff (Set x) (Set y) = list2set $ x \\ y

{- There's three different properties we can test for set Union -}
{- 1. A ⊆ (A ∪ B) -}
{- 2. B ⊆ (A ∪ B) -}
{- 3. (((A ∪ B) // B) // A) == empty set -}
unionProp1, unionProp2, unionProp3 :: (Ord a) => Set a -> Set a -> Bool
unionProp1 x y = subSet x $ setUnion x y
unionProp2 x y = subSet y $ setUnion x y
unionProp3 x y = isEmpty $ setDiff (setDiff (setUnion x y) y) x

unionPropChecker :: Set Int -> Set Int -> Bool
unionPropChecker x y = unionProp1 x y && unionProp2 x y && unionProp3 x y

randomUnionChecker :: IO Bool
randomUnionChecker = do
    x <- randomSet
    y <- randomSet
    return $ unionPropChecker x y

{- There's two different properties we can test for set Intersection -}
{- 1. (A ∩ B) ⊆ A-}
{- 2. (A ∩ B) ⊆ B-}
intersecProp1, intersecProp2 :: Set Int -> Set Int -> Bool
intersecProp1 x y = subSet (setIntersection x y) x
intersecProp2 x y = subSet (setIntersection x y) y

intersecPropChecker :: Set Int -> Set Int -> Bool
intersecPropChecker x y = intersecProp1 x y && intersecProp2 x y

randomIntersectionChecker :: IO Bool
randomIntersectionChecker = do
    x <- randomSet
    y <- randomSet
    return $ intersecPropChecker x y

{- There's two different properties we can test for set Difference -}
{- 1. (A // B) ⊆ A-}
{- 2. (B // A) ⊆ B-}
differenceProps1, differenceProps2 :: Set Int -> Set Int -> Bool
differenceProps1 x y = subSet (setDiff x y) x
differenceProps2 x y = subSet (setDiff y x) y

differencePropChecker :: Set Int -> Set Int -> Bool
differencePropChecker x y = differenceProps1 x y && differenceProps2 x y

randomDifferenceChecker :: IO Bool
randomDifferenceChecker = do
    x <- randomSet
    y <- randomSet
    return $ differencePropChecker x y

quickCheckDiff = quickCheck differencePropChecker
quickCheckUnion = quickCheck unionPropChecker
quickCheckIntersection = quickCheck intersecPropChecker
