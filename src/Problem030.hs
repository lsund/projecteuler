module Problem030 where

splitnum 0 = []
splitnum n = mod n 10 : splitnum (div n 10)

digitForthPowerSum x = sum (map (^4) (splitnum x))
digitFifthPowerSum x = sum (map (^5) (splitnum x))

sumOfDigitsNumbers 1000000 = []
sumOfDigitsNumbers x =
    if digitFifthPowerSum x == x
        then x : sumOfDigitsNumbers (x + 1)
        else sumOfDigitsNumbers (x + 1)



