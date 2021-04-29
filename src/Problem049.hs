module Problem049 where

import Data.List as List
import Data.List.Ordered
import qualified Data.Set as Set

intOrder :: (Integral a, Num a1) => a -> a1
intOrder 0 = 0
intOrder x = 1 + intOrder (div x 10)

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x : ys | x <- xs, ys <- permutation (delete x xs)]

primes :: [Int]
primes = eratos [2..]
    where
        eratos []     = []
        eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])

int2ListRev :: Int -> [Int]
int2ListRev 0 = []
int2ListRev x = mod x 10 : int2ListRev (div x 10)

int2List :: Int -> [Int]
int2List = reverse . int2ListRev

list2IntRev :: [Int] -> Int
list2IntRev []         = 0
list2IntRev (x : xs)   = x + (10 * list2IntRev xs)

list2Int :: [Int] -> Int
list2Int = list2IntRev . reverse

int2Set :: Int -> Set.Set Int
int2Set = Set.fromList . int2List

isPrime :: Int -> Bool
isPrime n = n `member` primes

fourDigitPrimes :: [Int]
fourDigitPrimes =
    let ltfours = takeWhile ((< (5 :: Int)) . intOrder) primes
    in dropWhile ((< (4 :: Int)) . intOrder) ltfours

numpermutations :: Int -> [Int]
numpermutations = map list2IntRev .  permutation . int2ListRev

isNumPermutationOf :: Int -> Int -> Bool
isNumPermutationOf n c = int2Set n == int2Set c

primePermutations :: Int -> [Int]
primePermutations n = filter (isNumPermutationOf n) fourDigitPrimes

unique :: Eq a => [a] -> Bool
unique xs = 1 == (length . List.nub) xs

isSolution :: (Eq a, Num a) => Set.Set a -> Bool
isSolution s = unique $ zipWith (-) (tail xs) xs
    where xs = Set.toList s

candidates :: [[Int]]
candidates = filter ((<) 4 . length) $ map primePermutations fourDigitPrimes

onlyLists :: Int -> Set.Set a -> Set.Set (Set.Set a)
onlyLists n s
    | n == 0                    = Set.singleton Set.empty
    | Set.size s < n || n < 0   = error "onlyLists: out of range n"
    | Set.size s == n           = Set.singleton s
    | otherwise                 = Set.fromDistinctAscList . map Set.fromDistinctAscList $ go n (Set.size s) (Set.toList s)
      where
        go 1 _ xs = map return xs
        go k l (x:xs)
            | k == l = [x:xs]
            | otherwise = map (x:) (go (k-1) (l-1) xs) ++ go k (l-1) xs

hasSolution :: (Num a, Ord a) => [a] -> Bool
hasSolution xs = foldl (\acc x -> isSolution x || acc) False $ (Set.toList) $ onlyLists 3 $ Set.fromList xs

