module Problem005 where

evendiv 0 = [-1]
evendiv x = map (mod x) [20, 19, 18, 17, 16, 15, 14, 13, 12, 11]
{-evendiv x = map (mod x) [10, 9, 8, 7, 6]-}

smallmult x =
	let
		z = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
		{-z = [0, 0, 0, 0, 0]-}
	in
		if evendiv x == z then x else smallmult (x + 20)


solve :: IO ()
solve = do
	let res = smallmult 0
	putStrLn $ "The result is: " ++ show res

