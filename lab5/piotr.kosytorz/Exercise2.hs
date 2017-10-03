module Exercise2 where

-- | Exercise1
-- | ===========================================================================
-- | Time spent: 2h

-- | Note: modifications are marked and explained
-- | Note2: report at the bottom of the file
-- | Note3: NRC modifications are commented. You can enable them by
-- | removing comments from lines that start with "--[nrc]"

import Data.List
import System.Random

type Row    = Int
type Column = Int
type Value  = Int
type Grid   = [[Value]]

-- | Modification begin
-- | ===========================================================================
-- | Definitions from Exercise2
type Position = (Row,Column)
type Constrnt = [[Position]]
-- | ===========================================================================
-- | Modification end

positions, values :: [Int]
positions = [1..9]
values    = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

-- | Modification begin
-- | ===========================================================================
-- | nrcBlocks - is an adaptation of blocks of 3 rows and 3 columns anchored in
-- | points: (2,2), (2,6), (6,2), (6,6)
-- | ===========================================================================
--[nrc] nrcBlocks :: [[Int]]
--[nrc] nrcBlocks = [[2..4],[6..8]]
-- | ===========================================================================
-- | Modification end

-- | Modification begin
-- | ===========================================================================
-- | Constrains from Exercise2
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
-- | NRC constraint (to show the difference)
-- | ===========================================================================
--[nrc] nrcConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]
-- | ===========================================================================
-- | Modification end

-- | Helper
-- | ===========================================================================
-- | union of elements of a list
unite:: (Eq a) => [[a]] -> [a]
unite = foldl1 union

-- | Modification begin
-- | ===========================================================================
-- | list of constrains
constrnts = [rowConstrnt, columnConstrnt, blockConstrnt {--[nrc] ,nrcConstrnt --} ]
-- | ===========================================================================
-- | Modification end


showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s =
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ]

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c)
  where
  pos :: [[a]] -> (Row,Column) -> a
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks

-- | Modification begin
-- | ===========================================================================
-- | nrcBl - returns the NRC block to which a given element (x) belong
-- | ===========================================================================
--[nrc] nrcBl :: Int -> [Int]
--[nrc] nrcBl x = concat $ filter (elem x) nrcBlocks
-- | ===========================================================================
-- | Modification end

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) =
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r =
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c =
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =
  (freeInRow s r)
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c))

-- | Modification begin
-- | ===========================================================================
-- | Constrains from Exercise2
freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let
    ys = filter (elem (r,c)) xs
  in
    foldl1 intersect (map ((values \\) . map s) ys)
-- | ===========================================================================
-- | Modification end

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (y,z) x = if x == y then z else f x

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         prune (r,c,v) constraints) | v <- vs ]

prune :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune (r,c,v) rest
-- | Modification begin
-- | ===========================================================================
-- | sameNrcBlock - checks if two elem are in the same nrc block
-- | ===========================================================================
--[nrc]   | sameNrcBlock (r,c) (x,y) =
--[nrc]         (x,y,zs\\[v]) : prune (r,c,v) rest
-- | ===========================================================================
-- | Modification end
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y

-- | Modification begin
-- | ===========================================================================
-- | sameNrcBlock - checks if two elem are in the same nrc block
-- | ===========================================================================
--[nrc] sameNrcBlock :: (Row,Column) -> (Row,Column) -> Bool
--[nrc] sameNrcBlock (r,c) (x,y) = nrcBl r == nrcBl x && nrcBl c == nrcBl y
-- | ===========================================================================
-- | Modification end

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in
              if (not . consistent) s then []
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,
                            c <- positions,
                            s (r,c) == 0 ]

-- | compares lengt of lists (that are the 3rd element of the two touples in )
length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraints :: Sudoku -> [Constraint]
constraints s = sortBy length3rd
-- | Modification begin
-- | ===========================================================================
--  [(r,c, freeAtPos s (r,c)) |
  [(r,c, freeAtPos' s (r,c) (unite constrnts) ) | -- constrnts is defined at the top of the file
-- | ===========================================================================
-- | Modification end
      (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node])
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs)
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8],
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

-- | Puzzle (from Exercise1)
-- | ===========================================================================
puzzle :: Grid
puzzle = [[0,0,0,3,0,0,0,0,0],
          [0,0,0,7,0,0,3,0,0],
          [2,0,0,0,0,0,0,0,8],
          [0,0,6,0,0,5,0,0,0],
          [0,9,1,6,0,0,0,0,0],
          [3,0,0,0,7,1,2,0,0],
          [0,0,0,0,0,0,0,3,1],
          [0,8,0,0,4,0,0,0,0],
          [0,0,2,0,0,0,0,0,0]]
-- | ===========================================================================
-- | end puzzle

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs
                  if null y
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs)
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs
                        then return []
                        else return
                          (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node])
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes =
  do xs <- ionodes
     if null xs
       then return []
       else
         if goal (head xs)
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys
                      then return [head ys]
                      else if null (tail xs) then return []
                           else
                             rsearch
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s)
  where s = eraseS (fst n) (r,c)

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s

-- | Report1: Extendability comparision
-- | ===========================================================================
-- | The definitions proposed by students make the code easier to extend.
-- | I.ex. extension to NRC needs only an extra constraint:
-- | nrcConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]
-- | and an update of the prune method. All modifications for NRC are in comments.
-- | The original version demands more modifications of code.

-- | Report1: Effectivity comparision
-- | ===========================================================================
-- | I performed 3 tests for each version (the original one and the refactored one)
-- | Results are represented in the table below:
-- |
-- +-------------------+-------------+-------------+
-- | Original          | Refactored  |             |
-- +-------------------+-------------+-------------+
-- | Test1 time        | 2,22        | 1,86        |
-- +-------------------+-------------+-------------+
-- | Test2 time        | 1,46        | 1,55        |
-- +-------------------+-------------+-------------+
-- | Test3 time        | 1,56        | 1,52        |
-- +-------------------+-------------+-------------+
-- |                   |             |             |
-- +-------------------+-------------+-------------+
-- | Test1 memory      | 1446988448  | 1276985640  |
-- +-------------------+-------------+-------------+
-- | Test2 memory      | 1027144448  | 1095394032  |
-- +-------------------+-------------+-------------+
-- | Test3 memory      | 1066689728  | 1073891952  |
-- +-------------------+-------------+-------------+
-- |                   |             |             |
-- +-------------------+-------------+-------------+
-- | Avg time          | 1,746666667 | 1,643333333 |
-- +-------------------+-------------+-------------+
-- | Avg memory        | 1180274208  | 1148757208  |
-- +-------------------+-------------+-------------+
-- |                   |             |             |
-- +-------------------+-------------+-------------+
-- | Difference time   | 6%          |             |
-- +-------------------+-------------+-------------+
-- | Difference memory | 3%          |             |
-- +-------------------+-------------+-------------+
-- |
-- | The measured difference after 3 tests are between 3% and 6%
-- | We can assume thus, that the difference is nihil and the versions are
-- | roughly the same efficient.
-- |
-- | ===========================================================================
-- | Original test results
-- | ===========================================================================
-- |
-- | Original version:
-- Prelude> :l Lecture5.hs
-- [1 of 1] Compiling Lecture5         ( Lecture5.hs, interpreted )
-- Ok, modules loaded: Lecture5.
-- (0.27 secs,)
-- *Lecture5> main
-- +-------+-------+-------+
-- | 3 9 6 | 1 4 7 | 2 8 5 |
-- | 2 5 4 | 3 6 8 | 7 1 9 |
-- | 1 7 8 | 5 9 2 | 4 3 6 |
-- +-------+-------+-------+
-- | 7 6 3 | 8 2 4 | 5 9 1 |
-- | 4 1 5 | 6 3 9 | 8 7 2 |
-- | 9 8 2 | 7 1 5 | 3 6 4 |
-- +-------+-------+-------+
-- | 8 4 7 | 9 5 6 | 1 2 3 |
-- | 6 2 1 | 4 7 3 | 9 5 8 |
-- | 5 3 9 | 2 8 1 | 6 4 7 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- |     6 | 1     | 2     |
-- |   5   | 3     |   1   |
-- |       |   9   | 4     |
-- +-------+-------+-------+
-- |   6   |   2 4 | 5     |
-- | 4     |       | 8 7   |
-- | 9     |       |       |
-- +-------+-------+-------+
-- |     7 | 9     |       |
-- |       |     3 |     8 |
-- |   3   |   8 1 |   4   |
-- +-------+-------+-------+
-- (2.22 secs, 1,446,988,448 bytes)
-- *Lecture5> main
-- +-------+-------+-------+
-- | 4 8 2 | 6 3 9 | 1 5 7 |
-- | 1 6 5 | 7 8 4 | 9 3 2 |
-- | 9 3 7 | 1 5 2 | 8 4 6 |
-- +-------+-------+-------+
-- | 6 9 1 | 3 2 7 | 4 8 5 |
-- | 7 5 3 | 8 4 1 | 2 6 9 |
-- | 2 4 8 | 9 6 5 | 3 7 1 |
-- +-------+-------+-------+
-- | 8 1 4 | 2 7 6 | 5 9 3 |
-- | 3 7 9 | 5 1 8 | 6 2 4 |
-- | 5 2 6 | 4 9 3 | 7 1 8 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- |       | 6 3   |   5   |
-- |       |   8   | 9   2 |
-- | 9     |   5 2 | 8     |
-- +-------+-------+-------+
-- |       |   2 7 |   8   |
-- |       |       | 2   9 |
-- |   4 8 |       | 3     |
-- +-------+-------+-------+
-- |       |       |       |
-- |   7   | 5   8 | 6     |
-- |     6 | 4     |   1   |
-- +-------+-------+-------+
-- (1.46 secs, 1,027,144,448 bytes)
-- *Lecture5> main
-- +-------+-------+-------+
-- | 7 6 5 | 8 4 2 | 1 3 9 |
-- | 3 2 4 | 9 6 1 | 5 8 7 |
-- | 1 9 8 | 5 7 3 | 2 4 6 |
-- +-------+-------+-------+
-- | 9 4 6 | 1 3 5 | 8 7 2 |
-- | 2 7 1 | 6 8 4 | 9 5 3 |
-- | 8 5 3 | 7 2 9 | 6 1 4 |
-- +-------+-------+-------+
-- | 6 3 2 | 4 1 8 | 7 9 5 |
-- | 4 8 9 | 2 5 7 | 3 6 1 |
-- | 5 1 7 | 3 9 6 | 4 2 8 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- |       |   4 2 |     9 |
-- |     4 |     1 |       |
-- |   9   | 5     |     6 |
-- +-------+-------+-------+
-- |       |       |   7 2 |
-- | 2   1 | 6     |     3 |
-- | 8     |     9 |       |
-- +-------+-------+-------+
-- |     2 |       | 7     |
-- |       |   5   |   6   |
-- |   1 7 | 3   6 |   2   |
-- +-------+-------+-------+
-- (1.56 secs, 1,066,689,728 bytes)
-- |
-- | Refactored version:
-- |
-- *Lecture5> :l Exercise2
-- [1 of 1] Compiling Exercise2        ( Exercise2.hs, interpreted )
-- Ok, modules loaded: Exercise2.
-- (0.14 secs,)
-- *Exercise2> main
-- +-------+-------+-------+
-- | 7 6 2 | 4 3 5 | 9 8 1 |
-- | 3 4 1 | 9 8 7 | 5 6 2 |
-- | 9 8 5 | 1 2 6 | 4 3 7 |
-- +-------+-------+-------+
-- | 5 7 8 | 2 9 4 | 3 1 6 |
-- | 6 2 4 | 7 1 3 | 8 5 9 |
-- | 1 9 3 | 6 5 8 | 7 2 4 |
-- +-------+-------+-------+
-- | 8 1 7 | 5 4 2 | 6 9 3 |
-- | 4 3 9 | 8 6 1 | 2 7 5 |
-- | 2 5 6 | 3 7 9 | 1 4 8 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- | 7     |     5 |     1 |
-- | 3     |   8   |       |
-- |       | 1     |   3 7 |
-- +-------+-------+-------+
-- | 5 7   |       |     6 |
-- | 6     |       | 8     |
-- |   9   |     8 | 7   4 |
-- +-------+-------+-------+
-- |       |     2 |   9   |
-- | 4     |       | 2     |
-- |   5 6 | 3     |       |
-- +-------+-------+-------+
-- (1.86 secs, 1,276,985,640 bytes)
-- *Exercise2> main
-- +-------+-------+-------+
-- | 1 8 7 | 2 5 4 | 9 6 3 |
-- | 2 6 4 | 3 9 7 | 5 1 8 |
-- | 9 3 5 | 6 1 8 | 7 4 2 |
-- +-------+-------+-------+
-- | 5 2 3 | 8 7 1 | 4 9 6 |
-- | 7 4 6 | 9 3 2 | 8 5 1 |
-- | 8 9 1 | 4 6 5 | 2 3 7 |
-- +-------+-------+-------+
-- | 3 7 2 | 5 4 6 | 1 8 9 |
-- | 6 5 8 | 1 2 9 | 3 7 4 |
-- | 4 1 9 | 7 8 3 | 6 2 5 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- |       |     4 |   6 3 |
-- |       |   9   |       |
-- | 9     | 6     |   4   |
-- +-------+-------+-------+
-- |     3 |       |     6 |
-- |   4   |     2 | 8 5 1 |
-- |       |     5 | 2     |
-- +-------+-------+-------+
-- |       |       | 1     |
-- | 6 5   |       |   7   |
-- |     9 | 7 8   |       |
-- +-------+-------+-------+
-- (1.55 secs, 1,095,394,032 bytes)
-- *Exercise2> main
-- +-------+-------+-------+
-- | 6 4 3 | 1 7 9 | 5 8 2 |
-- | 5 1 2 | 4 3 8 | 9 7 6 |
-- | 9 8 7 | 2 5 6 | 4 1 3 |
-- +-------+-------+-------+
-- | 1 6 9 | 5 2 4 | 8 3 7 |
-- | 8 3 5 | 7 9 1 | 6 2 4 |
-- | 7 2 4 | 8 6 3 | 1 5 9 |
-- +-------+-------+-------+
-- | 2 5 8 | 6 4 7 | 3 9 1 |
-- | 3 7 6 | 9 1 5 | 2 4 8 |
-- | 4 9 1 | 3 8 2 | 7 6 5 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- |     3 |     9 |   8 2 |
-- |       | 4 3   |       |
-- | 9   7 |   5 6 |       |
-- +-------+-------+-------+
-- |       |   2   |       |
-- |     5 |       | 6   4 |
-- | 7 2   |       | 1   9 |
-- +-------+-------+-------+
-- |       | 6     |       |
-- |   7   |   1   | 2   8 |
-- | 4 9   |       | 7     |
-- +-------+-------+-------+
-- (1.52 secs, 1,073,891,952 bytes)
