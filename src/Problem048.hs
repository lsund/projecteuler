
module Problem048 where

getLast10 [a, b, c, d, e, f, g, h, i, j] = [a, b, c, d, e, f, g, h, i, j]
getLast10 (x : xs) = getLast10 xs

f xs = map (\x -> (x ^ x)) xs

solve :: IO ()
solve = do
        let x = getLast10 $ show $ foldl (+) 0 (map (\x -> x ^ x) [1..1000])
        putStrLn $ "result: " ++  show x
