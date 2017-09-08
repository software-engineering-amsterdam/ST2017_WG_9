module Ex7 (int2list, list2int, doubleEvery2nd, sumDigitsInTwoDigits, luhn, isAmericanExpress, isMaster, isVisa) where

import Data.List

{--
  Author: Piotr kosytorz
  Time spent: 2h (1h learinig and implementation, 1 hour tests)
--}

{-- int to array of ints --}
int2list :: Integer -> [Int]
int2list = map (read . (:[])) . show -- https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell#3964069

{-- concatenates numbers and creates one big int --}
list2int :: [Int] -> Integer
list2int = read . concatMap show

{-- doubles every second element of the list --}
doubleEvery2nd :: [Int] -> [Int]
doubleEvery2nd = zipWith ($) (cycle [id,(*2)]) -- https://stackoverflow.com/questions/28256830/apply-a-function-to-every-second-element-in-a-list/28258023#28267231

{-- runs through the list and sums the digit of every non-one-digit number  --}
sumDigitsInTwoDigits  :: [Int] -> [Int]
sumDigitsInTwoDigits = map (\x -> if x > 9 then (1 + x `mod` 10) else x)

{-- checks whether given integer passes the luhn test or not --}
luhn :: Integer -> Bool
luhn n = (sum $ sumDigitsInTwoDigits $ doubleEvery2nd $ reverse $ int2list n) `mod` 10 == 0

{-- checks whether the number is an Amex or not --}
{-- AMEX: pasees the luhn test, has 15 digits, starts with 3 and the second digit is 4 or 7 --}
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n
  && (length $ int2list n) == 15
  && (head (int2list n) == 3)
  && (int2list n!!1 == 4 ||int2list n!!1 == 7)

{-- checks whether the number is a MasterCard or not --}
{-- MasterCard: pasees the luhn test, has 16 digits, the firs two digit are between 51 and 55 or 222100 and 272099 --}
isMaster :: Integer -> Bool
isMaster n = luhn n
  && (length $ int2list n) == 16
  && (
    (( list2int $ take 2 $ int2list n) > 50 && (list2int $ take 2 $ int2list n) < 56)
    || (( list2int $ take 6 $ int2list n) > 222099 && (list2int $ take 6 $ int2list n) < 272100)
    )

{-- checks whether the number is a VISA or not --}
{-- VISA: pasees the luhn test, has 13, 16 or 19 digits and starts with 4  --}
isVisa :: Integer -> Bool
isVisa n = luhn n
  && (head (int2list n) == 4)
  && (
    (length $ int2list n) == 13
    || (length $ int2list n) == 16
    || (length $ int2list n) == 19
    )

{- tuples (card number, name of the card) -}
cardNumbers =
  [(4138290806873877, "visa"),
  (4532292630493635, "visa"),
  (4485161889117444273, "visa"),
  (4138920806873877, "twisted visa"),
  (4532292603493635, "twisted visa"),
  (4485161881917444273, "twisted visa"),
  (5181553479949321, "master"),
  (5101376312833867, "master"),
  (2221001421223775, "master"),
  (5181553497949321, "twisted master"),
  (5101376312833867, "twisted master"),
  (2221001241232775, "twisted master"),
  (340034812633509, "american"),
  (346238738204881, "american"),
  (345792985120183, "american"),
  (430034812633509, "twisted american"),
  (346285738204881, "twisted american"),
  (345792985210183, "twisted american"),
  (6011425921299114, "none"),
  (6011201441370531, "none"),
  (6011053485136010881, "none"),
  (5486932741637215, "none"),
  (5513531370209531, "none"),
  (5459228907728424, "none"),
  (5038102857031776, "none"),
  (6761358616460257, "none"),
  (6762973411644704, "none")]

{--
  All 3 tests iterate over the list, if name matches and result of is* function
  is positive, then there comes a True to the list. Once everything is tested,
  the whole test will be considered successful if all single tests are successful,
  therefore or([True, ...]) should return a Bool: whether the function
  processed all cases correctly or not.
 --}

testVisa :: [(Integer, [Char])] -> Bool
testVisa testData =
  or $ map (\x -> (snd x == "visa" && isVisa(fst x)) || (snd x /= "visa")) testData

testMaster :: [(Integer, [Char])] -> Bool
testMaster testData =
  or $ map (\x -> (snd x == "master" && isMaster(fst x)) || (snd x /= "master")) testData

testAmerican :: [(Integer, [Char])] -> Bool
testAmerican testData =
  or $ map (\x -> (snd x == "american" && isAmericanExpress(fst x)) || (snd x /= "american")) testData

{-- Perfirming the tests --}
test1 = testVisa cardNumbers
test2 = testMaster cardNumbers
test3 = testAmerican cardNumbers
