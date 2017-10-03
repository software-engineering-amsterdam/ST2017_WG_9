
module Exercise5

where

import Data.List
import System.Random
import Exercise1

-- Time spent: 30m
-- 15m on haskell and its stupid parsing errors because of indentation

{-
  The rewritten code from Exercise1 already complies to the NRC specs.

  The code down below is to check wether a generated sudoku can be solved.
-}

blab :: IO [()]
blab = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
          solveAndShow (sud2grid (fst s))
