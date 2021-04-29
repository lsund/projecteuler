module Problem027 where

import Data.Map as Map
import Data.List as List

lim :: Int
lim =999

ps = [2, 3, 7, 11]

zero :: Int -> Bool
zero 0 = True
zero _ = False

divisibleBy :: Int -> [Int] -> Bool
divisibleBy _ []       = False
divisibleBy n (x : xs) = zero (mod n x) || divisibleBy n xs

isprime :: Int -> [Int] -> Map Int Bool -> (Map Int Bool, Bool)
isprime k ps map
    | k < 0 = (map, False)
    | divisibleBy k ps = (map, False)
    | member k map = (map, map ! k)
    | otherwise    =
        let
            flag = Prelude.null [y | y <- [2..floor (sqrt (fromIntegral k))], mod k y == 0]
        in
            (Map.insert k flag map, flag)

quadratic :: Int -> Int -> Int -> Int
quadratic n a b = (n * n) + a * n + b

primecounter :: Int -> Int -> Int
primecounter a b =
    let
        primecounter' :: Int -> Int -> Int -> Map Int Bool -> Int
        primecounter' a b acc m =
                let
                    (m', flag) = isprime (quadratic acc a b) ps m
                in
                    if flag
                        then primecounter' a b (acc + 1) m'
                    else
                        acc
    in
        primecounter' a b 0 (Map.insert 2 True (Map.insert 3 True Map.empty))

quadraticPrimes' :: Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
quadraticPrimes' a  b (max, amax, bmax)
    | b == (-lim) = (max, amax, bmax)
    | a == (-lim) = quadraticPrimes' lim (b - 1) (max, amax, bmax)
    | otherwise   =
        let
            seqLen = primecounter a b
        in
            if seqLen > max then
                quadraticPrimes' (a - 1) b (seqLen, a, b)
            else
                quadraticPrimes' (a - 1) b (max, amax, bmax)

quadraticPrimes :: (Int, Int, Int)
quadraticPrimes = quadraticPrimes' lim lim (0, lim, lim)

solve :: IO ()
solve = do
    let (max, amax, bmax) = quadraticPrimes
    putStrLn $ "Max sequence length: " ++ show max
    putStrLn $ "a: " ++ show amax ++ " b: " ++ show bmax
    putStrLn $ "Prod: " ++ show (amax * bmax)

