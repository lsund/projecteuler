module Problem023 where

import Data.Maybe
import Data.List

isprime x = null [y | y <- [2..div x 2], mod x y == 0]

divisors x =
    let
        divs 1 _ = []
        divs n cand =
            if isprime cand && mod n cand == 0
                then cand : divs (div n cand) 2
                else divs n (cand + 1)
    in
        foldr (\x acc -> (foldl (*) 1 x) : acc) [] (subsequences $ divs x 2)


slowdiv n = [y | y <- [1..div n 2], mod n y == 0]

{-isAbundant n = sum (divisors n) > n-}

isAbundant n = sum [y | y <- [1..div n 2], mod n y == 0] > n

abundantNumbers 12  =   [12]
abundantNumbers max =   if isAbundant max
                            then max : abundantNumbers (max - 1)
                            else abundantNumbers (max - 1)

sumofTwoAbundant n =
    let candidates = abundantNumbers $ n - 12
    in setSum n candidates

setSum target candidates =
    let
        setSum' _ [] _ _ = Nothing
        setSum' target (c : cs) chosen []           = setSum' target cs c cs
        setSum' target  candidates chosen (x : xs)  =
            if target - chosen - x == 0
                then Just (chosen, x)
                else setSum' target candidates chosen xs
    in
        setSum' target candidates (head candidates) (tail candidates)
