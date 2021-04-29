module Problem033 where

import Data.Ratio

magnitude 0 = 0
magnitude n = 1 + magnitude (div n 10)

numberToList 0 = []
numberToList n = (rem n 10) : numberToList (div n 10)

firstDigit n = x where [_, x] = numberToList n
secondDigit n = x where [x, _] = numberToList n

ratio2Decimal r = (fromIntegral (numerator r)) / (fromIntegral (denominator r))

-- Is digit cancelling fraction
isDCF a b =
    mod a 10 /= 0 &&
    mod b 10 /= 0 &&
    (%) (firstDigit a) (secondDigit b) == r &&
    (secondDigit a) == (firstDigit b) &&
    magnitude a == 2 &&
    magnitude b == 2 &&
    ratio2Decimal r < 1 &&
    magnitude (numerator r) == 1 &&
    magnitude (denominator r) == 1
        where r = (%) a b

minNum :: Int
minNum = 11
maxNum :: Int
maxNum = 99

fAux a b acc
    | b > maxNum = acc
fAux a b acc
    | a > maxNum = fAux minNum (b + 1) acc
fAux a b acc
    | isDCF a b = fAux (a + 1) b ((a, b) : acc)
fAux a b acc = fAux (a + 1) b acc

prod =
    let
        dcfs = fAux minNum minNum []
        n = foldl (*) 1 (map fst dcfs)
        d = foldl (*) 1 (map snd dcfs)
    in
        (%) n d

