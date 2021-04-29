module Problem025 where

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

diginum :: Integer -> Integer
diginum 0 = 0
diginum x = 1 + diginum (div x 10)

tryfibs x =
    if diginum fib == 1000
        then x + 1
        else tryfibs (x + 1)
    where fib = fibs !! x

solve :: IO ()
solve = do
    let x = tryfibs 1
    putStrLn $ "The result is: " ++ show x
