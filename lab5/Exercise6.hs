module Exercise6 where

import Data.List
import Data.List.Split
import Lecture5

{--
Time: 1h 30m - Research, experiment with diffrent approaches, try out some
sudokus by hand

Research: "Difficulty Rating of Sudoku Puzzles: An Overview and Evaluation"
"Figure 3 shows a difference among several
specific instances – it shows that for easy problem there are
many possibilities for progress in each step whereas for hard
problem there are only few of them"
-->
Easier sudoku problems have less possibilities per step. If we have a few possible
values per empty field (values per contraint), we have an easy sudoku problem.
Hard sudoku problems give few hints and have a high amount of possibilities.

There are many metrics and formulas to categorize sudoku puzzels. To really
measure the complexity sodoku you need a large group of subjects or a artifical
intelligence that solves sudokus like an average sudoku player and then measure
the time and mental energy (compare glucose-level in blood) needed.

Example:
*Exercise6> groupContraintsLength exampleConstraints
[(1,6),(6,2),(9,5),(17,3),(21,4)]
--> There are 6 empty fields with 2 possibilities, 1 with 6 possibilities and so on.

*Exercise6> getContraintsDifficultyLevel exampleConstraints
198
--> Adding up all the possibilities: A small number indicates that the free fields
can only be filled with a limited number of digits

The next step was to gather difficulty data for sudoku problems:
*Exercise6> gatherDifficultyData 100 []
[(222,57),(250,59),(212,56),(222,56),(239,58),(216,57),(219,57),(190,55),(213,56),(209,56),(205,55),(213,56),(218,56),(223,57),(218,56),(221,57),(237,59),(221,57),(234,58),(226,57),(208,55),(220,56),(216,57),(237,58),(231,58),(197,55),(213,56),(227,57),(204,56),(224,57),(219,57),(215,56),(246,58),(224,57),(214,56),(224,56),(201,56),(212,56),(220,56),(225,57),(213,56),(218,57),(223,58),(219,57),(221,56),(235,58),(222,56),(231,58),(191,55),(233,57),(209,56),(208,56),(215,57),(240,58),(229,58),(217,56),(211,56),(211,56),(213,56),(240,58),(229,59),(228,57),(224,58),(218,56),(222,56),(223,57),(205,56),(219,57),(210,57),(215,56),(203,55),(239,58),(212,57),(230,58),(222,57),(213,56),(190,54),(216,55),(209,55),(227,58),(240,58),(222,58),(219,57),(226,57),(214,56),(219,56),(241,59),(219,57),(205,55),(236,58),(223,56),(208,56),(232,57),(220,58),(219,56),(196,54),(234,59),(213,57),(202,55),(239,58)]
(146.47 secs, 114,081,196,080 bytes)

[(difficulty level, length of constraints / amount of free fields)]

Lastly we split the difficulty data up in three groups
*Exercise6> splitDifficultyData difficultyData 3
[(190,214),(215,223),(223,246)]

Easy: difficulty from 190 to 214
Medium: difficulty from 215 to 223
Hard: difficulty from 223 to 246

------------------------------------------------
Resulting output:
*Exercise6> mainEx6
+-------+-------+-------+
| 1 6 4 | 8 7 3 | 9 5 2 |
| 3 5 8 | 2 9 4 | 6 1 7 |
| 7 2 9 | 5 6 1 | 8 3 4 |
+-------+-------+-------+
| 5 1 3 | 9 8 7 | 4 2 6 |
| 8 9 6 | 4 2 5 | 1 7 3 |
| 2 4 7 | 1 3 6 | 5 8 9 |
+-------+-------+-------+
| 4 3 1 | 6 5 2 | 7 9 8 |
| 6 8 2 | 7 1 9 | 3 4 5 |
| 9 7 5 | 3 4 8 | 2 6 1 |
+-------+-------+-------+
+-------+-------+-------+
|       | 8     | 9   2 |
| 3   8 |       |   1   |
|   2   |     1 |     4 |
+-------+-------+-------+
|   1   |       | 4   6 |
|     6 |     5 |       |
|       |   3   | 5 8   |
+-------+-------+-------+
|     1 |   5   |   9   |
| 6     |     9 |     5 |
|       | 3   8 |       |
+-------+-------+-------+
Difficulty: "Middle (222)"

--}

exampleConstraints :: [Constraint]
exampleConstraints = [(2,2,[2,8]),(3,1,[2,4]),(4,2,[4,8]),(6,7,[3,4]),(6,9,[4,7]),(7,5,[2,9]),(1,4,[2,3,7]),(2,3,[2,7,8]),(2,6,[2,3,9]),(2,8,[3,7,9]),(3,4,[2,7,9]),(3,6,[2,6,9]),(4,3,[4,5,8]),(4,4,[3,4,9]),(5,3,[2,4,5]),(6,6,[2,3,4]),(6,8,[3,4,7]),(8,1,[2,4,5]),(8,5,[2,7,9]),(8,8,[4,5,9]),(9,2,[2,4,8]),(9,5,[2,3,9]),(9,8,[4,5,9]),(1,2,[2,4,6,8]),(1,5,[2,3,6,7]),(1,7,[2,3,4,5]),(1,8,[3,4,5,7]),(2,9,[2,7,8,9]),(3,7,[1,2,4,9]),(3,8,[1,4,7,9]),(3,9,[2,4,7,9]),(4,1,[3,4,5,8]),(4,7,[3,4,5,9]),(4,9,[4,5,6,9]),(5,1,[2,3,4,5]),(5,4,[2,3,4,9]),(5,5,[2,3,6,9]),(5,9,[4,5,6,9]),(7,2,[1,2,4,8]),(7,4,[1,2,4,9]),(7,7,[2,4,5,9]),(8,6,[2,4,5,9]),(8,9,[2,4,5,9]),(9,1,[2,4,5,8]),(1,3,[2,4,6,7,8]),(1,9,[2,4,5,7,8]),(5,6,[2,3,4,6,9]),(5,7,[1,3,4,5,9]),(7,3,[2,4,5,8,9]),(7,6,[2,4,5,8,9]),(8,3,[2,4,5,6,9]),(8,4,[1,2,4,7,9]),(9,3,[2,4,5,8,9]),(9,6,[2,3,4,5,8,9])]

groupContraintsLength :: [Constraint] -> [(Int,Int)]
groupContraintsLength xs = sort (frequency ls)
  where ls = map (\(_,_,vs) -> length vs) xs


frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))

getContraintsDifficultyLevel :: [Constraint] -> Int
getContraintsDifficultyLevel cs = foldl (\c (a,l) -> c + a * l) 0 ls
  where ls = groupContraintsLength cs

gatherDifficultyData :: Int -> [(Int,Int)] -> IO ([(Int,Int)])
gatherDifficultyData 0 rs = return rs
gatherDifficultyData n rs =
  do
    [r] <- rsolveNs [emptyN]
    s <- genProblem r
    let
     cs = snd s
     dl = getContraintsDifficultyLevel cs
    gatherDifficultyData (n - 1) ((dl, length cs) : rs)

difficultyData :: [(Int,Int)]
difficultyData = [(222,57),(250,59),(212,56),(222,56),(239,58),(216,57),(219,57),(190,55),(213,56),(209,56),(205,55),(213,56),(218,56),(223,57),(218,56),(221,57),(237,59),(221,57),(234,58),(226,57),(208,55),(220,56),(216,57),(237,58),(231,58),(197,55),(213,56),(227,57),(204,56),(224,57),(219,57),(215,56),(246,58),(224,57),(214,56),(224,56),(201,56),(212,56),(220,56),(225,57),(213,56),(218,57),(223,58),(219,57),(221,56),(235,58),(222,56),(231,58),(191,55),(233,57),(209,56),(208,56),(215,57),(240,58),(229,58),(217,56),(211,56),(211,56),(213,56),(240,58),(229,59),(228,57),(224,58),(218,56),(222,56),(223,57),(205,56),(219,57),(210,57),(215,56),(203,55),(239,58),(212,57),(230,58),(222,57),(213,56),(190,54),(216,55),(209,55),(227,58),(240,58),(222,58),(219,57),(226,57),(214,56),(219,56),(241,59),(219,57),(205,55),(236,58),(223,56),(208,56),(232,57),(220,58),(219,56),(196,54),(234,59),(213,57),(202,55),(239,58)]

splitDifficultyData :: [(Int, Int)] -> Int -> [(Int,Int)]
splitDifficultyData ds n = map (\i -> (minimum (dsChunks !! i), maximum (dsChunks !! i))) [0..(n -1)]
  where
      ds2 = sort (map fst ds)
      dsChunks = chunksOf (length ds `div` n) ds2

getSudokuDifficultyLevel :: [Constraint] -> String
getSudokuDifficultyLevel cs
    | d < snd (ds !! 0) = "Easy (" ++ show d ++ ")"
    | d < snd (ds !! 1) = "Middle (" ++ show d ++ ")"
    | otherwise = "Hard (" ++ show d ++ ")"
    where
      ds = splitDifficultyData difficultyData 3
      d = getContraintsDifficultyLevel cs

mainEx6 :: IO ()
mainEx6 = do [r] <- rsolveNs [emptyN]
             showNode r
             s  <- genProblem r
             showNode s
             putStrLn ("Difficulty: " ++ show (getSudokuDifficultyLevel (snd s)))
