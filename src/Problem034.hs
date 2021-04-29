module Problem034 where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

digits 0 = []
digits x = (mod x 10) : digits (div x 10)

isCurious n = sum (map fac (digits n)) == n

findCurious 100000 = []
findCurious n =
    if isCurious n
        then n : findCurious (n + 1)
        else findCurious (n + 1)

solve :: IO ()
solve = do
    putStrLn $ "Some numbers" ++ show (findCurious 0)
