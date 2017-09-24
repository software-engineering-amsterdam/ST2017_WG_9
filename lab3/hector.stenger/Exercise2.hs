module Exercise2 where

import Lecture3
import Exercise4 (generateBT, randomIndex)
import System.Random

{- Time: 1.5 hours -}

{- Returns random Props. -}
getTestSuite :: Int -> IO [Form]
getTestSuite 0 = return []
getTestSuite n = do
    nodes <- randomRIO(1,100)
    index <- randomIndex [1..100]
    current <- generateBT nodes [1..index]
    next <- getTestSuite (n-1)
    return (current:next)

{- Check whether parsing the string representation yields back the same result. Similiar to double -}
{- applying rot13. -}
parseShow :: Form -> Bool
parseShow f = ([f] == (parse $ show f))

{- Test generates a 1000 different props and uses these props in the function above. Each props  -}
{- should evaluate in the parseShow function back to itself. -}
test :: IO Bool
test = do
    forms <- getTestSuite 1000
    return (and $ map parseShow forms)
