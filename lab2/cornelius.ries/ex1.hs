module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Time Spent: 2h
-- learning about IO
-- implement function

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)

getCounts :: [Float] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
getCounts [] c = c
getCounts (x:xs) (a,b,c,d)
  | 0 <= x && x <= 0.25 = getCounts xs (a+1,b,c,d)
  | 0.25 <= x && x <= 0.5 = getCounts xs (a,b+1,c,d)
  | 0.5 <= x && x <= 0.75 = getCounts xs (a,b,c+1,d)
  | otherwise = getCounts xs (a,b,c,d+1)

test n = do
      randoms <- probs n
      let
        (a,b,c,d) = getCounts randoms (0,0,0,0)
      return (a,b,c,d)
