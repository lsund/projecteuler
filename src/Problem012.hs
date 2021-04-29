module Problem012 where

import Data.Map as Map

triangleNumber :: Int -> Int
triangleNumber x = (x * (x + 1)) `div` 2

primify :: Int -> [Int]
primify x =
    let divide x i | x == i = [i]
        divide x i =
            if mod x i == 0
                then i : divide (div x i) 2
                else divide x (i + 1)
    in
        divide x 2

countUniqueElements :: [Int] -> Int -> Int -> [Int]
countUniqueElements [] _ count = [count]
countUniqueElements (x : xs) e count =
    if x /= e
        then count : countUniqueElements xs x 1
        else countUniqueElements xs e (count + 1)

numberOfDivisors :: [Int] -> Int
numberOfDivisors [] = 1
numberOfDivisors (x : xs) = (x + 1) * numberOfDivisors xs

divisors :: Int -> Int
divisors n =
    let primes = primify n
        first = head primes
        xs = countUniqueElements primes first 0
    in  numberOfDivisors (xs)

highDivisibleTriangularNumber :: Int -> Int
highDivisibleTriangularNumber i =
    let trinum = triangleNumber i
    in  if divisors trinum > 500
            then trinum
            else highDivisibleTriangularNumber (i + 1)

solve :: IO ()
solve = do
    let d = highDivisibleTriangularNumber 3
    putStrLn $ "The result is: " ++ show d
