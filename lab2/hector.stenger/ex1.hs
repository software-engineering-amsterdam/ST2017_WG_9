import System.Random
{- Time: 30 minutes -}

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

{- By far the easiest and least time consuming (for me, the programmer) solution I could -}
{- think of. You could extract another function from it, called filterRange which would -}
{- take an upper and lower bound. The only thing that isn't nice is the runtime, which is 4 -}
{- times as high when you do a single loop. But the runtime difference for this problem is not -}
{- problematic so we can get away with being 'lazy'. Otherwise I'd suggest something like a -}
{- single loop in which recursion is used to feed and increment the bucket counter. -}
buckets :: [Float] -> [Int]
buckets n = [first, second, third, fourth]
    where first = length (filter (\x -> x > 0 && x < 0.25) n)
          second = length (filter (\x -> x >= 0.25 && x < 0.5) n)
          third = length (filter (\x -> x >= 0.5 && x < 0.75) n)
          fourth = length (filter (\x -> x >= 0.75 && x < 1) n)

test = do
    probsFloats <- probs 10000
    return $ buckets probsFloats
