module Problem026 where

multOrd :: Integer -> Integer -> Integer -> Integer
multOrd a b k | mod (a ^ k) b == 1 = k
multOrd a b k                      = multOrd a b (k + 1)

isPrime :: Integer -> Bool
isPrime k = null [x | x <- [2..k - 1], mod k x  == 0]

isCoprime10 x = (x /= 2) && (x /= 5) && (isPrime x)

findLongestCycle :: Integer -> Integer -> Integer -> Integer
findLongestCycle 1000 y z    = z
findLongestCycle x    y z | (isCoprime10 x) && (o > y) = findLongestCycle (x + 1) o x where o = multOrd 10 x 1
findLongestCycle x    y z    = findLongestCycle (x + 1) y z

