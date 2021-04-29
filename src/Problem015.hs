module Problem015 where

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

combination :: Integer -> Integer -> Integer
combination n k = div (factorial n) (factorial k * factorial (n - k))


solve :: IO ()
solve = do
    let x = combination 40 20
    putStrLn $ "The result is: " ++ show x
