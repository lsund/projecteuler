module Problem016 where

digisum :: Integer -> Integer -> Integer
digisum 0 acc = acc
digisum x acc = digisum (div x 10) (acc + mod x 10)

solve :: IO ()
solve = do
    let num = 2 ^ 1000
    let x = digisum (num) 0
    putStrLn $ "The result is : " ++ show x
