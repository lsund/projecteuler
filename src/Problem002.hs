
module Problem002 where

import Data.List (tail)

-- update 2021-11-22
solution2 = sum $ filter even $ takeWhile (< 4000000) fibs

fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

collectvals :: [Int] -> Int -> [Int]
collectvals l i = if l !! i > 4000000 then [] else l !! i : collectvals l (i + 1)

geteven :: [Int] -> [Int]
geteven [] = []
geteven (x : xs) = if mod x 2 == 0 then x : geteven xs else geteven xs

sumlist :: [Int] -> Int
sumlist [] = 0
sumlist (x : xs) = x + sumlist xs

solution = sumlist (geteven (collectvals fibs 0))
