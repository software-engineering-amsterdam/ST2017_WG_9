import Data.List
import Data.Char
import Test.QuickCheck
{--
  Time spent: 1h 
--}

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{--
  Specification for ROT13:
  ========================

  Let be given the order set of english alphabet letters: alphabet = abcdefghijklmnopqrstuvwxyz
  and a string s that consist given letters and white spaces

  rot(s) is a new string p, where all the letters from s are replaced by a
  letter that's 13 letters to their right in the alphabet.
  If we there are less that 13 letters to the right for a given letter then we
  continue counting from the beginning of the alphabet.

  Because latin alphabet has 26 letters, a correct implementation of rot13 should
  satisfy the following: rot13(rot13(s)) = s

--}

{-- Implementation ROT13 --}

{-- For implementation here I will use the default ascii table avaialable in haskell --}

shiftStep = ord 'a' -- 97 in ascii table is the length that I am going to substract from converted chars
alphabetLastElementIndex = (ord 'z') - shiftStep -- 25, because I am indexing from 0
alphabetLength = alphabetLastElementIndex + 1 -- 26

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (x:xs)
  | not (x `elem` ['a'..'z']) = x : rot13 xs
  | otherwise = chr(((ord x - shiftStep + 13) `mod` alphabetLength)+shiftStep) : rot13 xs

{-- Tests --}

simpleText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse vitae risus justo. Duis efficitur sagittis augue, a suscipit sapien rhoncus convallis"

-- length s1 = length s2
lengthComp :: Positive Int -> Bool
lengthComp (Positive n) = n <= length simpleText --> length (take n simpleText) == length (rot13 (take n simpleText))
test1 = quickCheckResult lengthComp
{-- Results:
  *Main> test1
  +++ OK, passed 100 tests.
  Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
--}

-- rot13(rot13(s)) = s
refComp :: Positive Int -> Bool
refComp (Positive n) = n <= length simpleText --> rot13 (take n simpleText) == rot13 (rot13 (take n simpleText))
test2 = quickCheckResult lengthComp
{-- Results:
  *Main> test2
  +++ OK, passed 100 tests.
  Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
--}
