module Problem020 where

factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x - 1)

digisum :: Integer -> Integer
digisum 0 = 0
digisum x = (mod x 10) + digisum (div x 10)

solve :: IO ()
solve = do
    let x = digisum $ factorial 100
    putStrLn $ "The result is: " ++ show x
