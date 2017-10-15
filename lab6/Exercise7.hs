module Exercise7 where

-- | Exercise7
-- | ===========================================================================
-- | Time spent: 3h

-- | Note: Discussion and application run report at the bottom of this file

import Data.List
import Data.List.Split
import Data.Char
import System.Random
import Lecture6

-- | Facts:
-- | 2^n takes n+1 bits to represent, so one "1" and n "0"
-- | (2^(n+1))-1 is the biggest number that can be represented witn (n+1) bits, and
-- | its representation is n+1 "1"s.
-- |
-- | Finding: anything between 2^n and (2^(n+1))-1 has (n+1) bits.

-- | Generate primes of the binary length of n
generatePrime :: Int -> IO (Integer)
generatePrime n = do
  r <- randomRIO(2^(n-1), (2^n)-1)
  t <- primeMR 5 r                  -- | Miller-Rabin primality check with with k=5 (should be enough)
  if t then
    return r
  else
    generatePrime n

-- | Finds a pair of primes of the binary length of n
findAPair :: Int -> IO (Integer,Integer)
findAPair n = do
  p1 <- generatePrime n
  p2 <- generatePrime n
  if p1 /= p2
  then return (p1,p2)
  else findAPair n

-- | Transfroms string into an integer (reversable operation)
string2integer :: String -> Integer
string2integer s = read ("1" ++ (intercalate "" $ map (\x -> makeLen3 x)  $ map (show) (str2intlist s))) :: Integer
  where
  str2intlist s = map (toInteger . fromEnum ) s   -- | Takes every letter and gets it ASCII code, then creates a list
  makeLen3 x                                      -- | fills up leading zeros for strings shorter than 3
    | length x == 1 = "00" ++ x
    | length x == 2 = "0" ++ x
    | otherwise = x

-- | Transforms integer into a string
integer2string :: Integer -> String
integer2string i = intlist2str $ map (read) $ chunksOf 3 $ tail (show i)
  where
  intlist2str encoded = map ( chr . fromInteger ) encoded

-- | RSA algorithm (https://simple.wikipedia.org/wiki/RSA_(algorithm)):
-- | Demonstration of working
-- | ===========================================================================

main :: IO ()
main = do

  -- | Let's encrypt!
  -- | Bob wants to send Alice a secret message.
  -- | Alice has to send bob her public key, so she has to generate it first:

  pair <- findAPair 256           -- | Finding a pair of 256 bites length
  let p = fst pair                -- | Splitting up the tuple
      q = snd pair
      pubKey = rsaPublic p q      -- | Alice's public key
      privKey = rsaPrivate p q    -- | Alice's private key

  print $ "Public key: " ++ show pubKey
  print $ "Private key: " ++ show privKey

  -- | Now Alice sends her public key to Bob.
  -- |            ==========>
  -- | Bob has reveived Alice's public key.

  -- | Bob writes a message
  let message = "Hi Alice! I am moving to Ibiza!"
  print $ "The secret message is: " ++ message

  -- | And converts it into a number
  let m = string2integer message
  print $ "Original message in number format: " ++ show m

  -- | Now with Alice's public key, Bob encodes his message
  let c = rsaEncode pubKey m
  print $ "The cipher is: " ++ show c

  -- | Bob sends now his cipher to Alice
  -- |            ==========>
  -- | Alice has reveived Bob's encrypted message

  -- | Alice docedes the message with her private key:
  let dm = rsaDecode privKey c
  print $ "The decoded message in number format is: " ++ show dm

  -- | And eventually transforms the integer back to string:
  let bobsMessage = integer2string dm
  print $ "Bob's decoded message: " ++ bobsMessage

-- | Discussion
-- | ===========================================================================
-- | This implementation encodes correctly only messages which binary
-- | representation is not longer than 256 bits.
-- | If Bob's message would be bigger, that it should be chunked into pieces
-- | of max 256 bits.
-- | Length of the prime numbers is essential, because it guarantees the quality
-- | of encryption. The bigger the primes, the stronger the cipher.

-- | Application run report
-- | ===========================================================================
-- | *Exercise7> main
-- | "Public key: (3,5690315299888247336167662688167133770181686066225564909134896826276487719710779563378586760934978962421995250692970103210351051130791335756736762562008271)"
-- | "Private key: (3793543533258831557445108458778089180121124044150376606089931217517658479807085172963240516360643966943743707047112033246444128594586976406745328844448667,5690315299888247336167662688167133770181686066225564909134896826276487719710779563378586760934978962421995250692970103210351051130791335756736762562008271)"
-- | "The secret message is: Hi Alice! I am moving to Ibiza!"
-- | "Original message in number format: 1072105032065108105099101033032073032097109032109111118105110103032116111032073098105122097033"
-- | "The cipher is: 4509348583439676319782943476096848329928309689788130547572100801542446677145061846460195110897686023787839706676089413636668837474724209836129919689836019"
-- | "The decoded message in number format is: 1072105032065108105099101033032073032097109032109111118105110103032116111032073098105122097033"
-- | "Bob's decoded message: Hi Alice! I am moving to Ibiza!"
