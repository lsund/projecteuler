
module Problem006 where

sumsquare 0 = 0
sumsquare x = (x ^ 2) + sumsquare (x - 1)

squaresum x = sum [1..x] ^ 2

solve :: IO ()
solve = do
	let res = abs (squaresum 100 - sumsquare 100)
	putStrLn $ "The result is: " ++ show res
