module Library where

import Data.List
import SetOrd

setLength :: Set a -> Int
setLength (Set xs) = length xs
