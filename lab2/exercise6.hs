module Exercise6 where

import Data.Char
import Data.List
import Test.QuickCheck


{--
Time: 45m

Specification of ROT13:
- ROT13 substitutes each character in a message with the character that comes
13 letters later in the alphabet
- If the substition is outside of the array starts from the beginning
- ROT13 supports the english alphabet (13 characters), if it is applied twice
the original message appears again (13*2 = 26)
- ROT13 does not touch characters that do not belong to the english alphabet

Source: https://en.wikipedia.org/wiki/ROT13

Tests:

To test the specification from above can test 3 properties:
testReversal: Test if ROT13 returns the same string if applied twice
testNotRotateNonAlphabetChars: Test if ROT13 does not affect chars outside of
                               the alphabet
testRotateAlphabeticChars: Test if ROT13 only affects chars that are inside the
                           alphabet

Example test run with quickCheck:
*Exercise6 Data.List> quickCheck testReversal
+++ OK, passed 100 tests.
*Exercise6 Data.List> quickCheck testNotRotateNonAlphabetChars
+++ OK, passed 100 tests.
*Exercise6 Data.List> quickCheck testRotateAlphabeticChars
+++ OK, passed 100 tests.
*Exercise6 Data.List>

--}

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

{------------------------------------------------------------------------------
                              IMPLEMENTATION
-------------------------------------------------------------------------------}

rot13 :: String -> String
rot13 m = rotateString m 13

{--
rotateString: Flexible function that allows to convert all Caesar ciphers. Not
only the ROT13 special case.
--}
rotateString :: String -> Int -> String
rotateString m n = map (\c ->  rotateChar c n) m

rotateChar :: Char -> Int -> Char
rotateChar c n
  | c `elem` alphabet =  getCharAtPosition (findIndex (== c) alphabet) n
  | otherwise = c

{--
getCharAtPosition: Helper function to retrieve the value inside the monad and
throw and error if the character is not inside the alphabet (should never be
the case because rotateChar checks if the character is an element of the
alphabet)
--}
getCharAtPosition :: Maybe Int -> Int -> Char
getCharAtPosition Nothing _ = error "Invalid character in string."
getCharAtPosition (Just p) n = alphabet !! p1
  where p1 = (p + n) `mod` length alphabet

{------------------------------------------------------------------------------
                                    TESTS
-------------------------------------------------------------------------------}

testReversal :: String -> Bool
testReversal s = (rot13 . rot13) s == s

testNotRotateNonAlphabetChars :: String -> Bool
testNotRotateNonAlphabetChars s = rot13 fs == fs
  where fs = filter (`notElem` alphabet) s

testRotateAlphabeticChars :: String -> Bool
testRotateAlphabeticChars s = all (\(c1,c2) -> c1 /= c2) z
  where
    fs = filter (`elem` alphabet) s
    rfs = rot13 fs
    z = zip fs rfs
