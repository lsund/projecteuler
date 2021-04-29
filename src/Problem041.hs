
module Problem041 where

import Data.List

list2IntRev :: [Int] -> Int
list2IntRev []         = 0
list2IntRev (x : xs)   = x + (10 * list2IntRev xs)

list2Int :: [Int] -> Int
list2Int = list2IntRev . reverse

isPrime :: Int -> Bool
isPrime 1 = False
isPrime k = Prelude.null [y | y <- [2..floor (sqrt (fromIntegral k))], mod k y == 0]

permutation [] = [[]]
permutation xs = [x : ys | x <- xs, ys <- permutation (delete x xs)]

largePandigitals = map list2Int $ permutation [7,6..1]

largestPandigitalPrime = find isPrime $ take 100 largePandigitals

