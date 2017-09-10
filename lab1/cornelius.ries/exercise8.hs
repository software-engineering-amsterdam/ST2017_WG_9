module Lab1 where
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew boy = boy /= Carl && boy /= Matthew
accuses Peter boy = boy == Matthew || boy == Jack
accuses Jack boy = not (accuses Matthew boy) && not (accuses Peter boy)
accuses Arnold boy = accuses Matthew boy /= accuses Peter boy
accuses Carl boy = not (accuses Arnold boy)


accusers :: Boy -> [Boy]
accusers boy = [ x | x <- boys, accuses x boy ]

accusers2 :: Boy -> [Boy]
accusers2 boy = filter (\x -> accuses x boy) boys

guilty :: [Boy]
guilty = [x | x <- boys, length(accusers x) == 3]

guilty2 :: [Boy]
guilty2 = filter (\x -> length(accusers x) == 3) boys

honest :: [Boy]
honest = accusers Jack

honest2 :: [Boy]
honest2 = filter (\x -> accuses x Jack) boys
