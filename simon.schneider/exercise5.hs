module Exercise5 where

{--function to find primes with Eratosthenes’ sieve taken from lecture --}
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

eprimes :: Integer -> [Integer]
eprimes n = sieve [2..n]

{-- consecutivePrimes: Does not only return primes that are the sum
of n consecutive prime numbers, but also the consecutive row itself. --}
consecutivePrimes :: Int -> [Integer] -> [([Integer], Integer)]
consecutivePrimes _ [] = []
consecutivePrimes n l
  | length l < n = []
  | s `elem` l = (h, s) : r
  | otherwise = r
  where
    s = sum h
    h = take n l
    r = consecutivePrimes n (tail l)

{-- Find the first prime that is the sum of 101 consecutive primes:
    Result:
    ([83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,
    181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,
    283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,
    409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,
    523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,
    647,653,659,661,673,677],37447)
    --> 37447
 --}
consecutivePrime101 :: ([Integer], Integer)
consecutivePrime101 = head ( consecutivePrimes 101 (sieve [2..100000]))
