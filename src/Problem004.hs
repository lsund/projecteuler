
module Problem004 where

digits 0 = []
digits x = x `mod` 10 : digits(x `div` 10)

split l =
	let
		first = take (length l `div` 2) l
		second =
			if length l `mod` 2 == 0 then
				drop (length l `div` 2) l
			else
				drop (length l `div` 2 + 1) l
	in
		[first, reverse(second)]

ispalindrome x =
	let
		parts = split (digits x)
	in
		head(parts) == head(tail(parts))

mult (m, 0, largest) = mult (m - 1, 999, largest)
mult (0, n, largest) = largest
mult (m, n, largest) =
	if ispalindrome (m * n) && m * n > largest then
		mult (m, n - 1, m * n)
	else
		mult (m, n - 1, largest)

solve = do
	let num = 9908099
	let res = mult (999, 999, 0)
	putStrLn $ "The result is: " ++ show res
