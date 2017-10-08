module Exercise5 where

import Exercise1

{------------------------------------------------------------------------------
Time spent: 1h 20m (Read Lecture5 code, understand NRC problem, adjust)

TASK:
Extend the code of the lectures to create a program that generates NRC Sudoku
problems, that is, Sudoku problems satisfying the extra constraint explained in
the NRC exercise above.
Deliverables:
[x] NRC Sudoku generator
[x] indication of time spent.

ANSWER:
This was already solved in exercise1, since all relevant functions were adjusted
to recognice the extra NRC constraint.

EXAMPLE:
*Exercise5> generateAndShowNrc
+-------+-------+-------+
| 4 7 3 | 9 2 8 | 5 6 1 |
| 1 9 8 | 6 4 5 | 7 3 2 |
| 6 2 5 | 7 3 1 | 8 4 9 |
+-------+-------+-------+
| 5 1 4 | 3 7 6 | 9 2 8 |
| 8 6 2 | 5 9 4 | 3 1 7 |
| 7 3 9 | 8 1 2 | 4 5 6 |
+-------+-------+-------+
| 2 5 7 | 1 8 3 | 6 9 4 |
| 9 4 6 | 2 5 7 | 1 8 3 |
| 3 8 1 | 4 6 9 | 2 7 5 |
+-------+-------+-------+
+-------+-------+-------+
|       |       | 5     |
|       | 6 4   |       |
|       |       |     9 |
+-------+-------+-------+
|       | 3     |       |
|   6 2 |       |   1 7 |
| 7 3   | 8     |       |
+-------+-------+-------+
|       |       | 6     |
|   4   |       |       |
|     1 | 4     |       |
+-------+-------+-------+

We can clearly see the 4 NRC blocks and that they do not ignore the extra
constraint.
NRC block 1 from example above:
986
257
143 --> no doubled digits

-------------------------------------------------------------------------------}

generateAndShowNrc :: IO()
generateAndShowNrc = main
