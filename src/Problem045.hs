module Problem045 where

import Data.List.Ordered
import Data.List

triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2

pentagonal :: Int -> Int
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: Int -> Int
hexagonal n = n * (2 * n - 1)

unbounded :: (Int -> Int) -> [Int]
unbounded f = map f [1..]

triangulars :: [Int]
triangulars = unbounded triangular

pentagonals :: [Int]
pentagonals = unbounded pentagonal

hexagonals :: [Int]
hexagonals = unbounded hexagonal

isTriangular :: Int -> Bool
isTriangular n = member n triangulars

isPentagonal :: Int -> Bool
isPentagonal n = member n pentagonals

isHexagonal :: Int -> Bool
isHexagonal n = member n hexagonals

isTPH :: Int -> Bool
isTPH n = all ($ n) [isTriangular, isPentagonal, isHexagonal]

findTPHnums :: [Int]
findTPHnums = take 3 $ filter isTPH triangulars
