module Exercise2 where 

import qualified OriginalSudoku
import qualified RefactoredSudoku

{- The proposed refactor makes it easier to drop in more properties which have to hold -}
{- for a sudoku. I therefor would suggest using the drop in cause additions only need -}
{- to be added as a list item -}

{- Set a profiler flag in ghci using ':set +s' then alternate between these function -}
{- calls to see which one is faster. At the moment they are both using only 1 sudoku but -}
{- this could be change to more. I did not however use random sudoku's as entry
- because the profiler only tracks the time for function you directly call.
- Piping the same random sudoku's to different solvers would be a problem then-}
main :: IO [()]
main = do 
    solve1 <- OriginalSudoku.solveAndShow OriginalSudoku.example6
    {- solve1 <- RefactoredSudoku.solveAndShow RefactoredSudoku.example6 -}
    return solve1

{- Refactored run -}
{- *Exercise2> main -}
{- +-------+-------+-------+ -}
{- | 4 7 8 | 3 9 2 | 6 1 5 | -}
{- | 6 1 9 | 7 5 8 | 3 2 4 | -}
{- | 2 3 5 | 4 1 6 | 9 7 8 | -}
{- +-------+-------+-------+ -}
{- | 7 2 6 | 8 3 5 | 1 4 9 | -}
{- | 8 9 1 | 6 2 4 | 7 5 3 | -}
{- | 3 5 4 | 9 7 1 | 2 8 6 | -}
{- +-------+-------+-------+ -}
{- | 5 6 7 | 2 8 9 | 4 3 1 | -}
{- | 9 8 3 | 1 4 7 | 5 6 2 | -}
{- | 1 4 2 | 5 6 3 | 8 9 7 | -}
{- +-------+-------+-------+ -}
{- [()] -}
{- (0.04 secs, 19,236,208 bytes) -}

{- Original run. -}
{- *Exercise2> main -}
{- +-------+-------+-------+ -}
{- | 4 7 8 | 3 9 2 | 6 1 5 | -}
{- | 6 1 9 | 7 5 8 | 3 2 4 | -}
{- | 2 3 5 | 4 1 6 | 9 7 8 | -}
{- +-------+-------+-------+ -}
{- | 7 2 6 | 8 3 5 | 1 4 9 | -}
{- | 8 9 1 | 6 2 4 | 7 5 3 | -}
{- | 3 5 4 | 9 7 1 | 2 8 6 | -}
{- +-------+-------+-------+ -}
{- | 5 6 7 | 2 8 9 | 4 3 1 | -}
{- | 9 8 3 | 1 4 7 | 5 6 2 | -}
{- | 1 4 2 | 5 6 3 | 8 9 7 | -}
{- +-------+-------+-------+ -}
{- [()] -}
{- (0.03 secs, 19,561,912 bytes) -}
