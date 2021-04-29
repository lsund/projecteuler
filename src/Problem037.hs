module Problem037 where

import qualified Data.Map as Map

--------------------------------------------------------------------------------
--  Utilities

intOrder 0 = 0
intOrder x = 1 + intOrder (div x 10)

int2ListRev :: Int -> [Int]
int2ListRev 0 = []
int2ListRev x = mod x 10 : int2ListRev (div x 10)

int2List :: Int -> [Int]
int2List = reverse . int2ListRev

reverseInt :: Int -> Int
reverseInt = list2Int . int2ListRev

list2IntRev :: [Int] -> Int
list2IntRev []         = 0
list2IntRev (x : xs)   = x + (10 * list2IntRev xs)

list2Int :: [Int] -> Int
list2Int = list2IntRev . reverse

--------------------------------------------------------------------------------
--  Main

butFirst :: Int -> Int
butFirst n = n - (div n base * base) where base = 10 ^ (intOrder n - 1)

butLast :: Int -> Int
butLast n = div n 10

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p : xs) = p : sieve [x |x <- xs, x `mod` p > 0]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime k = Prelude.null [y | y <- [2..floor (sqrt (fromIntegral k))], mod k y == 0]

suffixes :: Int -> [Int]
suffixes 0 = []
suffixes n = n : suffixes (butFirst n)

prefixes :: Int -> [Int]
prefixes 0 = []
prefixes n = n : prefixes (butLast n)

isTruncable :: Int -> Bool
isTruncable n =
    let xs = (suffixes n ++ prefixes n)
        allPrime []             = True
        allPrime (x : xs)
            | not $ isPrime x   = False
            | otherwise         = allPrime xs
    in allPrime xs

truncablePrimes :: Int -> [Int]
truncablePrimes = truncablePrimes' [] (drop 4 primes) 0
  where
    truncablePrimes' :: [Int] -> [Int] -> Int -> Int -> [Int]
    truncablePrimes' tps (p : ps) len max
        | len == max            = tps
        | isTruncable p         = truncablePrimes' (p : tps) ps (len + 1) max
        | otherwise             = truncablePrimes' tps ps len max

solve = print $ truncablePrimes 11

