import Data.List
import Data.Char

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

-- length s1 = length s2

-- (string1, string2) - difference in ord x for corresponding letters of the alphabet from string1 and string2 must be always 13

-- rot13(rot13(s)) = s

-- rot should not touch anything that does not belong to the alphabet

-- rot should transform all letters that belong to the alphabet
