
module Problem018 where

merge :: [Int] -> [Int] -> [Int]
merge _ [] = []
merge (a : b : cs) (d : es) = (d + max a b) : merge (b : cs) es

flattenTriangle :: [[Int]] -> Int
flattenTriangle [x] = head x
flattenTriangle (a : b : cs) = flattenTriangle ((merge a b) : cs)


solve :: IO ()
solve = do
    let x = flattenTriangle [[8, 5, 9, 3], [2, 4, 6], [7, 4], [3]]
    putStrLn $ "result: " ++ show x

