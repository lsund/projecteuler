module Problem036 where

decToBin :: Int -> [Int]
decToBin 0 = [0]
decToBin 1 = [1]
decToBin num = (mod num 2) : decToBin (div num 2)

digits :: Int -> [Int]
digits 0 = []
digits x = mod x 10 : digits (div x 10)

split :: [Int] -> ([Int], [Int])
split x =
    let half = div (length x) 2
        first = take half x
        second = if mod (length x) 2 == 0 then drop half x else drop (half + 1) x
    in (first, second)

ispalindrome :: [Int] -> Bool
ispalindrome n = fst parts == reverse (snd parts) where parts = split n

doubleBasePalindrome :: Int -> Bool
doubleBasePalindrome n =
    let nbin = decToBin n
        ndec = digits n
    in ispalindrome ndec && ispalindrome nbin

collectDBPalindromes :: Int -> [Int]
collectDBPalindromes 0 = []
collectDBPalindromes x  | doubleBasePalindrome x = x : collectDBPalindromes (x - 1)
                        | otherwise = collectDBPalindromes (x - 1)
