--Let d(n) be the sum of all proper divisors of n
--Amicable numbers belongs to a pair (a, b), a =/b, such that d(a) == b && d(b) == a
--

module Problem021 where

-- The sum of all proper divisors of n
d n = foldl (+) 0 divisors where divisors = [y | y <- [1..div n 2], mod n y == 0]


-- The sum of all amicable numbers below 10000
checkAmicable 10000 = []
checkAmicable a =
    let b = d a
        c = d b
    in  if a == c && a /= b
            then a : checkAmicable (a + 1)
            else checkAmicable (a + 1)



solve :: IO ()
solve = do
    let x = checkAmicable 1
    putStrLn $ "Amicable numbers: " ++ show x
    putStrLn $ "Sum: " ++ show (foldl (+) 0 x)
