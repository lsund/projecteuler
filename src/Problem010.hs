module Problem010 where

isprime k = null [y | y <- [2..floor (sqrt (fromIntegral k))], mod k y == 0]

getprimes i =
	if i > 2000000 then 2 else
	if isprime i then
		i + getprimes (i + 2)
	else
		0 + getprimes (i + 2)

solve :: IO ()
solve = do
	let kay = getprimes 3
	putStrLn $ "And the result is..." ++ show kay
