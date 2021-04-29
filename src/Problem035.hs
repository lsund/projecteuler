module Problem035 where

import Data.List

isprime :: Int -> Bool
isprime k = null [y | y <- [2..floor (sqrt (fromIntegral k))], mod k y == 0]

littlecycle :: [Int] -> [Int]
littlecycle x = take (len * 2 - 1) (cycle x) where len = length x

circular :: [Int] -> [[Int]]
circular xs =
    let
        start = 0
        lc = littlecycle xs
        len = length xs
        circular' :: [Int] -> Int -> [[Int]]
        circular' xs i
            | i == len  =  []
            | otherwise =
                drop i (take (len + i) lc) : circular' xs (i + 1)
    in
        circular' xs start

toList :: Int -> [Int]
toList 0    = []
toList dig  = mod dig 10 : toList (div dig 10)

toDig :: [Int] -> Int
toDig xs =
    let
        startmult = 1
        toDig' :: [Int] -> Int -> Int
        toDig' [] _ = 0
        toDig'(x : xs) mul = mul * x + toDig' xs (mul * 10)
    in
        toDig' xs startmult

isCyclicPrime :: Int -> Bool
isCyclicPrime x =
    let l       = toList x
        circ    = circular l
        cycles  = map toDig circ
    in and (map isprime cycles)

cyclicPrimesUnder :: Int -> [Int]
cyclicPrimesUnder x =
    let startprime = 2
        cyclicPrimesUnder' :: Int -> [Int]
        cyclicPrimesUnder' start
            | start == x = []
            | otherwise =
                if isCyclicPrime start
                    then start : cyclicPrimesUnder' (start + 1)
                    else cyclicPrimesUnder' (start + 1)
    in cyclicPrimesUnder' startprime



