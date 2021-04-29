module Util where

import Data.List
import Data.List.Ordered
import Data.Char
import qualified Data.Set as Set

type Set = Set.Set


pairs' :: [t] -> [t] -> [(t, t)]
pairs' [] _              = []
pairs' (_ : xs) []       = pairs' xs (tail xs)
pairs' (x : xs) (y : ys) =
    (x, y) : pairs' (x : xs) ys


pairs :: [t] -> [(t, t)]
pairs xs = pairs' xs (tail xs)


intOrder :: (Integral a, Num a1) => a -> a1
intOrder 0 = 0
intOrder x = 1 + intOrder (div x 10)

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x : ys | x <- xs, ys <- permutation (delete x xs)]

-- Has to be upper case characters
charValue :: Char -> Int
charValue c = ord c - 64


int2ListRev :: Int -> [Int]
int2ListRev 0 = []
int2ListRev x = mod x 10 : int2ListRev (div x 10)


int2List :: Int -> [Int]
int2List = reverse . int2ListRev


int2Set :: Int -> Set Int
int2Set = Set.fromList . int2List


list2IntRev :: [Int] -> Int
list2IntRev []         = 0
list2IntRev (x : xs)   = x + (10 * list2IntRev xs)


list2Int :: [Int] -> Int
list2Int = list2IntRev . reverse


elements :: (Eq a) => [a] -> [a] -> Bool
elements xs ys = null (xs \\ ys)


isPandigital' :: [Int] -> Bool
isPandigital' xs = elements xs [1..n] && length xs == n
    where n = length xs


isPandigital :: Int -> Bool
isPandigital = isPandigital' . int2List


factor :: Int -> [Int]
factor = factor' primes
    where
        factor' _ 1          = []
        factor' (p : ps) n
            | n `mod` p == 0 = p : factor' (p : ps) (n `div` p)
            | otherwise      = factor' ps n
        factor' _ _          = []


isPrime :: Int -> Bool
isPrime n = n `member` primes


primes'' :: [Int]
primes'' = eratos [2..]
    where
        eratos []     = []
        eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])

primes' :: [Int]
primes' = eratos' [2..]
    where
        eratos' []     = []
        eratos' (p:xs) = p : eratos' (xs `minus` [p*p, p*p+p..])

primes :: [Int]
primes = 2 : oddprimes
  where
    oddprimes = sieve'' [3,5..] 9 oddprimes
    sieve'' [] _ _  = error "should not happen"
    sieve'' (x:xs) q ps@ ~(p:t)
        | x < q     = x : sieve'' xs q ps
        | otherwise =     sieve'' (xs `minus` [q, q+2*p..]) (head t^(2 :: Int)) t


