module Problem031 where

count :: [Int] -> Int -> Int
count _ 0                       = 1
count _ n | n < 0               = 0
count [] n                      = 0
count (x:xs) n                  = count xs n + count (x:xs) (n - x)


