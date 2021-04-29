module Problem032 where

import Data.List as List
import Data.Vector as Vector
import Data.Set as Set

magnitude 0 = 0
magnitude n = 1 + magnitude (div n 10)

numberToList 0 = []
numberToList n = (rem n 10) : numberToList (div n 10)

isPandigital a b =
    let
      curID = (numberToList a List.++ numberToList b List.++ numberToList (a * b))
      curIDSet = Set.fromList curID
      targetID = Set.fromList [1..9]
    in
        if (List.length curID) /= 9
            then False
        else
            (Set.intersection curIDSet targetID) == targetID

qualifies :: Int -> Int -> Vector Int -> Bool
qualifies a b vec
    |
    {-| (magnitude a + magnitude b) == 5 && -}
    {-magnitude c == 4 &&-}
    ((!) vec c) == 0 &&
    isPandigital a b = True where c = a * b
qualifies _ _ _ = False

maxNum = 9999
minNum = 0

third (a, b, c) = c

pandigitals :: Int -> Int -> Vector Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
pandigitals a b vec sum
    | a == maxNum = sum
pandigitals a b vec sum
    | b == maxNum  = pandigitals (a + 1) minNum vec sum
pandigitals a b vec sum
    | qualifies a b vec = pandigitals a (b + 1) ((//) vec [(c, 1)]) ((a, b, c) : sum)
    where c = a * b
pandigitals a b vec sum = pandigitals a (b + 1) vec sum

p = pandigitals 1 1 (Vector.replicate 100000000 0) []

pandigitalSum = List.sum (List.map third p)

