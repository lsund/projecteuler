module Problem067 where

import Data.List.Split


merge :: [Int] -> [Int] -> [Int]
merge _ [] = []
merge (a : b : cs) (d : es) = (d + max a b) : merge (b : cs) es

flattenTriangle :: [[Int]] -> Int
flattenTriangle [x] = head x
flattenTriangle (a : b : cs) = flattenTriangle ((merge a b) : cs)

tonum :: String -> [Int]
tonum xs = map read (splitOn " " xs)

tonums xs = init (map tonum xs)

solve :: IO ()
solve =  do
        contents <- readFile "../problemdata/p067_triangle.txt"
        let lines = reverse $ tonums $ splitOn "\n" contents
        let res = flattenTriangle lines
        putStrLn $ show res
