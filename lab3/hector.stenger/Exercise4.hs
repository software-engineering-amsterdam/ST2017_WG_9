module Exercise4 where

import System.Random
import Exercise1 (equiv)
import Lecture3

{- Time: 3 hours. -}

esrever = reverse

getUnary :: Int -> Int -> Form
getUnary n x
    | n == 0 = prop
    | otherwise = Neg prop
    where prop = Prop x

getRandomUnary :: Int -> [Int] -> IO Form
getRandomUnary x n = do
    p <- randomRIO(0,1)
    randomProp <- getRandomElement n
    return $ getUnary p randomProp

getRandomElement :: [Int] -> IO Int
getRandomElement xs = do
    p <- randomRIO(0, (length xs - 1))
    return (xs !! p)

{- My implementation always returns a binary predicate with 2 props. So for instance -}
{- Cnj(Props 1, Props 2). It's not Impossible to have something like Cnj(Props 1, Props 2, Props 3) -}
{- in my 'world' this would be Cnj(Cnj(Props 1, Props 2), Props 3). The chance this happens in my -}
{- implementation is only lower. -}
getBinary :: Int -> Form -> Form -> Form
getBinary n x y
    | n == 0 = Impl x y
    | n == 1 = Cnj [x,y]
    | n == 2 = Dsj [x,y]
    | otherwise = Equiv x y

{- Return random binary form -}
getRandomBinary :: IO Form -> IO Form -> IO Form
getRandomBinary x y = do
    p <- randomRIO(0,3)
    first <- x
    second <- y
    return $ getBinary p first second

{- Returns a random array index. -}
randomIndex :: [Int] -> IO Int
randomIndex n = randomRIO(1, (length n - 1))

generateNodes :: [Int] -> [Int] -> IO Form
generateNodes n [x] = do
    form <- getRandomUnary x n
    return form
generateNodes n l1@(x) = do
    index <- randomIndex l1
    let 
        lhs = generateNodes n (take index x)
        rhs = generateNodes n (take (length l1-index) (esrever x))
    form <- getRandomBinary lhs rhs
    return form

generateBT :: Int -> [Int] -> IO Form
generateBT n xs = generateNodes xs [1..n]

test :: IO Bool
test = do
    form <- generateBT 5 [1..5]
    return (form == (head $ parse $ show form))
