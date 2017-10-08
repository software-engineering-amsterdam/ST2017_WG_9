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
+--------+---------+--------+
| 2  4 7 | 8  9  6 | 5 1  3 |
|   +--------+  +--------+  |
| 9 |5 8 | 2 |1 |3 | 6 4 |7 |
|   |    |   |  |  |     |  |
| 6 |1 3 | 7 |4 |5 | 8 9 |2 |
+--------+---------+--------+
| 5 |6 4 | 9 |3 |7 | 1 2 |8 |
|   +--------+  +--------+  |
| 8  9 2 | 1  6  4 | 7 3  5 |
|   +--------+  +--------+  |
| 3 |7 1 | 5 |2 |8 | 4 6 |9 |
+--------+---------+--------+
| 7 |2 9 | 4 |8 |1 | 3 5 |6 |
|   |    |   |  |  |     |  |
| 4 |8 6 | 3 |5 |9 | 2 7 |1 |
|   +--------+  +--------+  |
| 1  3 5 | 6  7  2 | 9 8  4 |
+--------+---------+--------+

+--------+---------+--------+
|    4   |         | 5      |
|   +--------+  +--------+  |
| 9 |  8 |   |  |  | 6   |7 |
|   |    |   |  |  |     |  |
|   |  3 |   |4 |  |   9 |  |
+--------+---------+--------+
|   |    |   |  |  |     |  |
|   +--------+  +--------+  |
|        |         |      5 |
|   +--------+  +--------+  |
| 3 |  1 |   |  |  |     |  |
+--------+---------+--------+
|   |  9 | 4 |8 |  |     |  |
|   |    |   |  |  |     |  |
|   |    |   |  |  | 2   |  |
|   +--------+  +--------+  |
| 1      | 6       |        |
+--------+---------+--------+

We can clearly see the 4 NRC blocks and that they do not ignore the extra
constraint. (no double digits in NRC blocks)
NRC block 1 from example above:
582
137
649
--> no doubled digits

-------------------------------------------------------------------------------}

generateAndShowNrc :: IO()
generateAndShowNrc = main
