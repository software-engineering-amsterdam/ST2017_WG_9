module Exercise8 where

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew x = x /= Matthew && x /= Carl
accuses Peter x = (x == Matthew) || x == Jack

{--
accusers :: Boy -> [Boy]

guilty :: [Boy]

honest :: [Boy]
--}
