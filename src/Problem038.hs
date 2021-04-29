module Problem038 where

import Data.List as List

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

-- Checkt weather xs and ys contains the elements of ys
elements :: (Eq a) => [a] -> [a] -> Bool
elements xs ys = null (xs \\ ys)

isPandigital xs = elements xs [1..9] && length xs == 9

concatenatedProduct :: Int -> [Int] -> [Int]
concatenatedProduct n xs = List.concat $ List.map (int2List . (n *)) xs

largestConcatenatedProduct' :: Int -> Int -> Int -> [Int] -> Int
largestConcatenatedProduct' max x n ns
    | x == 100000 && length lcp > 9 = max
    | length lcp > 9 = largestConcatenatedProduct' max (x + 1) 2 [1..2]
    | cp > max && pd = largestConcatenatedProduct' cp x (n + 1) [1..(n + 1)]
    | otherwise      = largestConcatenatedProduct' max x (n + 1) [1..(n + 1)]
      where
        lcp = concatenatedProduct x ns
        cp  = list2Int lcp
        pd  = isPandigital lcp

largestConcatenatedProduct :: Int
largestConcatenatedProduct = largestConcatenatedProduct' 0 1 2 [1..2]
