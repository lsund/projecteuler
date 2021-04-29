module Problem042 where

import Data.Char
import Data.List.Split

nthTriangleNum :: Int -> Int
nthTriangleNum n = div (n * (n + 1)) 2

triangleNums :: [Int]
triangleNums = [nthTriangleNum x | x <- [1..]]

-- Has to be upper case characters
charValue :: Char -> Int
charValue c = ord c - 64

wordValue :: String -> Int
wordValue = foldr ((+) . charValue) 0

isTriangleWord :: String -> Bool
isTriangleWord = isTriangleWord' triangleNums
isTriangleWord' :: [Int] -> String -> Bool
isTriangleWord' (x : xs) w
    | wordValue w == x = True
    | wordValue w < x  = False
    | otherwise        = isTriangleWord' xs w

solve = do
    s <- readFile "p042_words.txt"
    let (w : ws) = splitOn "\",\"" s
        ws'      = "A" : init ws ++ ["YOUTH"]
        tws      = filter isTriangleWord ws'
    print $ length tws
    return ()

