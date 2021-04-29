
module Problem046 where

import Data.List.Ordered

primes :: [Int]
primes = eratos [2..]
    where
        eratos []     = []
        eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])

composites :: [Int]
composites = [2..] `minus` primes

oddComposites :: [Int]
oddComposites = filter odd composites

isPrime :: Int -> Bool
isPrime n = n `member` primes

primesUpto :: Int -> [Int]
primesUpto n = takeWhile (<= n) primes

twiceSquaresUpto :: Int -> [Int]
twiceSquaresUpto n = takeWhile (<=n) [2 * x^2 | x <- [1..]]

isPrimeSquareSum :: Int -> Bool
isPrimeSquareSum n = n `elem` [p + s | p <- primesUpto n, s <- twiceSquaresUpto n]


