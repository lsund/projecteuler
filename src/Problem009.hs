
module Problem009 where

-- Update 22-11-2021
solution = [a * b * c | a <- [1..498], b <- [succ a..498], c <- [succ b..498], a ** 2 + b ** 2 == c ** 2, a + b + c == 1000]

isTriplet a b c = a ^ 2 + b ^ 2 == c ^ 2

isCandidate a b c target =
	a + b + c == target && isTriplet a b c

test :: Int -> Int -> Int -> Int -> [Int]

test target 998 999 1000 = []
test target a 999 c = test target (a + 1) (a + 2) (a + 3)
test target a b 1000 = test target a (b + 1) (b + 2)
test target a b c =
	if isCandidate a b c target then
		[a, b, c]
	else
		test target a b (c + 1)

solve :: IO ()
solve =
	do
		let res = test 1000 0 1 2
		putStrLn $ "The result is: " ++ show res
