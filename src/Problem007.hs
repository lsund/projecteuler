module Problem007 where

isprime k = not (elem 0 (map (mod k) [2..div k 2]))

primes x i =
	if i == 10001 then
		x - 1
	else
		if isprime x then
			primes (x + 1) (i + 1)
		else
			primes (x + 1) i


solve :: IO ()
solve = do
	let res = primes 2 0
	putStrLn $ "The result is: " ++ show res
