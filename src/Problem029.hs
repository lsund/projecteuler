module Problem029 where

import Data.List

upper = 5

c :: Integer -> [Integer]
c b = map (\x -> x ^ b) [2..upper]

distinctPowers :: Integer -> Integer -> Int
distinctPowers lower upper =
    let distinctPowers' n   | n == upper = c upper
                            | otherwise = c n ++ distinctPowers' (n + 1)
    in length $ nub $ distinctPowers' lower


solve :: IO ()
solve = do
    let res = distinctPowers 2 upper
    putStrLn $ "result: " ++  show res
