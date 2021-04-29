module Problem043 where

import Data.List

solve = print (sum $ map list2Int $ filter isDigitDivisible (nPandigitals 9))

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

elements :: (Eq a) => [a] -> [a] -> Bool
elements xs ys = null (xs \\ ys)

isPandigital' :: [Int] -> Bool
isPandigital' xs = elements xs [1..n] && length xs == n
    where n = length xs

isPandigital :: Int -> Bool
isPandigital = isPandigital' . int2List

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x : ys | x <- xs, ys <- permutation (delete x xs)]

divisible :: Int -> Int -> Bool
divisible m n = rem m n == 0

nPandigitals n = permutation [0..n]

isDigitDivisible :: [Int] -> Bool
isDigitDivisible xs =
    let [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] = xs
        divs = [divisible (list2Int [d2, d3, d4]) 2
                ,divisible (list2Int [d3, d4, d5]) 3
                ,divisible (list2Int [d4, d5, d6]) 5
                ,divisible (list2Int [d5, d6, d7]) 7
                ,divisible (list2Int [d6, d7, d8]) 11
                ,divisible (list2Int [d7, d8, d9]) 13
                ,divisible (list2Int [d8, d9, d10]) 17]
    in and divs

