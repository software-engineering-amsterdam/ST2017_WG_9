module Exercise7 where

import Data.Char
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- Returns the last element of a list -}
lastElement :: [Integer] -> Integer
lastElement = head . reverse

{- Implementation of the Luhn algorithm from wikipedia.  -}
luhn :: Integer -> Bool
luhn n = sum moduloElements * 9 `mod` 10 == checkElement
    where ns = toIntegerArray n
          moduloElements = oddAndEven (reverse (init ns))
          checkElement = lastElement ns

{- Converts an Integer to an List filled with Integers. It works by casting the Integer to -}
{- a String followed by a map which parses the Char to an Int to an Integer. It's easier to -}
{- work with an Array of Integers since all the function signatures use Integers and Arrays -}
{- offer us the needed functionality (map sum reverse init elem etc). -}
toIntegerArray :: Integer -> [Integer]
toIntegerArray n = map (toInteger . digitToInt) (show n)

arrayToInteger :: [Integer] -> Integer
arrayToInteger n = read (concat (map show n))

onOdd :: Integer -> Integer
onOdd n
    | (n*2) > 9 = n*2-9
    | otherwise = n*2

{- 'Iterates' over the odd and even values by using pattern matching. To make the pattern -}
{- exhaustive we need to add a case for an Array with just a single element and one without -}
{- an element. The length of Visa / Master / AE don't share a similarity (could either be odd -}
{- or even) so we need to serve both needs. oddFun could be turned into an parameter but -}
{- this is not necesary in this case. -}
oddAndEven :: [Integer] -> [Integer]
oddAndEven [] = []
oddAndEven [x] = [onOdd x]
oddAndEven (x:y:xs) = (onOdd x) : y : oddAndEven xs

{- function signature from the lab assignment -}
isAmericanExpress, isMaster, isVisa :: Integer -> Bool

{- I have used the following website to check for card specific validation rules aswel as the -}
{- correct creditcard numbers. -}
{- https://www.freeformatter.com/credit-card-number-generator-validator.html#howToValidate -}

isAmericanExpress n = validator lengthValidator prefixValidator n
    where aEPrefix = [34, 37]
          aELength = [15]
          lengthValidator = (\n -> elem (length n) aELength)
          prefixValidator = (\n -> elem ((n!!0)*10 + n!!1) aEPrefix)

isMaster n = validator lengthValidator prefixValidator n
    where masterPrefix = [51, 52, 53, 54, 55] ++ [222100..272099]
          masterLength = [16]
          lengthValidator = (\n -> elem (length n) masterLength)
          prefixValidator = (\n -> elem (arrayToInteger(take 2 n)) masterPrefix || elem (arrayToInteger(take 6 n)) masterPrefix)

isVisa n = validator lengthValidator prefixValidator n
    where visaPrefix = [4]
          visaLength = [13, 16, 19]
          lengthValidator = (\n -> elem (length n) visaLength)
          prefixValidator = (\n -> elem (n!!0) visaPrefix)

{- Validating cardnumbers boils down to three checks. -}
{- 1. The length has to be of a specific Int, could be a list. -}
{- 2. The prefix has to be a specific number, could be one or two numbers from a list -}
{- 3. Luhn's algorithm has to be valid on the number -}
validator :: ([Integer] -> Bool) -> ([Integer] -> Bool) -> Integer -> Bool
validator lengthVal prefixVal arr = lengthVal cardnumbers && prefixVal cardnumbers && luhn arr
    where cardnumbers = toIntegerArray arr

{-
Since we do not have a simple credit card generator we are using examples from
http://www.getcreditcardnumbers.com

All the possibilities of either correct or incorrect cc numbers. Either the
length or the prefix is incorrect
-}
correctMasters :: [Integer]
correctMasters = [5519189879429607,
                  2221002283190788,
                  5290331028850599]

incorrectMasters :: [Integer]
incorrectMasters = [0519189879429607,
                    22210022831907888,
                    02903310288505998]

correctVisas :: [Integer]
correctVisas = [4916908882295818,
                4916908882295818,
                4929502543771104,
                4556110785253854913]

inCorrectVisas :: [Integer]
inCorrectVisas = [2916908882295818,
                  491690888229,
                  492950254377110,
                  49295025437711]
correctAE :: [Integer]
correctAE = [378014233128052,
             375391662844422,
             346374000541732]

incorrectAE :: [Integer]
incorrectAE = [388014233128052,
              385391662844422,
              3463740005417322]

aEIncorrectRunner = not (all isAmericanExpress incorrectAE)
aECorrectRunner = all isAmericanExpress correctAE

visaIncorrectRunner = not (all isVisa inCorrectVisas)
visaCorrectRunner = all isVisa correctVisas

masterIncorrectRunner = not (all isMaster incorrectMasters)
masterCorrectRunner = all isMaster correctMasters

cardRunner = (visaIncorrectRunner && visaIncorrectRunner)
    && (aEIncorrectRunner && aECorrectRunner)
    && (masterIncorrectRunner && masterCorrectRunner)

predicate :: Integer -> Bool
predicate n = n > 0

test :: IO()
test = verboseCheck(\n -> predicate n --> ((arrayToInteger . toIntegerArray) n == n))
