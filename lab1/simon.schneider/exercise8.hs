module Exercise8 where

{--
In this exercise we first decided to draw a matrix onto the whiteboard.
After figuring out how we can decide which one was the thief we started to
translate the predicate logic and the solution into haskell code.

The solution below finds exactly 1 guilty boy (Jack) and also finds out which of
the boys are actually lying.

--}

{--
Define "one of them but not both" as an operator, also known as xor
--}
xor :: Bool -> Bool -> Bool
xor a b = a /= b

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

{--
* Matthew: Carl didn't do it, and neither did I.
* Peter It was Matthew or it was Jack.
* Jack Matthew and Peter are both lying.
* Arnold Matthew or Peter is speaking the truth, but not both.
* Carl What Arnold says is not true.
--}
accuses :: Boy -> Boy -> Bool
accuses Matthew x = x /= Matthew &&  x /= Carl
accuses Peter x = x == Matthew || x == Jack
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = xor (accuses Matthew x) (accuses Peter x)
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers x = filter (\b -> accuses b x) boys

{--
guilty: Give the list of guilty boys:
Result: [Jack]
--}
guilty :: [Boy]
guilty = filter (\b -> length (accusers b) == 3)  boys

{--
accusesAll: A reuseable function that checks if a boy accuses all the boys in
the given list.
--}
accusesAll :: Boy -> [Boy] -> Bool
accusesAll x l = all (\b -> accuses x b) l

{--
Plus the list of boys who made honest (true) statements:
Result: [Matthew,Peter,Carl]
--}
honest :: [Boy]
honest = filter (\b -> accusesAll b guilty ) boys

{--
Test the following statements given by the text question:
* One of them has stolen something from some kid they all dislike
* three of these boys always tell the truth
* and two always lie
--}
testPreconditions :: Bool
testPreconditions = length honest == 3 &&
                    length guilty == 1 &&
                    length (accusers (head honest)) == 2
