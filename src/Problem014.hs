module Problem014 where

collatz :: Int -> Int -> Int
collatz 1 len = len + 1
collatz n len | n `mod` 2 == 0 = collatz (n `div` 2) (len + 1)
collatz n len = collatz (3 * n + 1) (len + 1)

iteratecollatz :: Int -> Int -> Int -> Int
iteratecollatz 1000000 maxchain maxnum = maxnum
iteratecollatz n maxchain maxnum =
    if chainlen > maxchain
        then iteratecollatz (n + 1) chainlen n
        else iteratecollatz (n + 1) maxchain maxnum
    where chainlen = collatz n 0

solve :: IO ()
solve = do
    let x = iteratecollatz 1 1 0
    putStrLn $ "The result is : " ++ show x
