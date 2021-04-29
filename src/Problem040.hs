module Problem040 where

int2ListRev :: Int -> [Int]
int2ListRev 0 = []
int2ListRev x = mod x 10 : int2ListRev (div x 10)

int2List = reverse . int2ListRev

concatIntList n = concatMap int2List [0..n]

champernownesConstant =
    let xs = 0 : concatIntList 1000000
        d1 = xs !! 1
        d10 = xs !! 10
        d100 = xs !! 100
        d1000 = xs !! 1000
        d10000 = xs !! 10000
        d100000 = xs !! 100000
        d1000000 = xs !! 1000000
    in
        d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
