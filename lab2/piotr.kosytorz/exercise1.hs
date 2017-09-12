import System.Random

{--
  Time spent: 2h (30 min for the problem and 90 min for figuring out how monads work)
--}

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

quartileCount :: IO [Float] -> IO [Int]
quartileCount ml = do
  l <- ml
  return [
    length $ filter (\x -> x<0.25) l,
    length $ filter (\x -> x>=0.25 && x<0.5) l,
    length $ filter (\x -> x>=0.5 && x<0.75) l,
    length $ filter (\x -> x>0.75) l
    ]

{-- Tests --}

addMargin :: Int -> Int -> Int -> Float
addMargin total pieces param =
  (((fromIntegral total) / (fromIntegral pieces) ) * (1 + ((fromIntegral param) / 100)))

-- cm = countings monad
-- mp = margin percentage (let's say 5)
checkBalance :: IO [Int] -> Int -> IO Bool
checkBalance cm mp = do
  countings <- cm
  return (all (\x -> (fromIntegral x) < addMargin (sum countings) 4 mp) countings)

test = checkBalance (quartileCount probs 10000) 5 -- 5% margin rate
