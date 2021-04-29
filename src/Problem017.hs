module Problem017 where

import Control.Exception.Base


lettercount :: Int -> Int
lettercount x   | elem x [1, 2, 6, 10]                  = 3
                | elem x [0, 4, 5, 9]                   = 4
                | elem x [3, 7, 8, 40, 50, 60]          = 5
                | elem x [11, 12, 20, 30, 80, 90]       = 6
                | elem x [15, 16, 70]                   = 7
                | elem x [13, 14, 18, 19]               = 8
                | x == 17                               = 9
                | x == 1000                             = 11
                | mod x 100 == 0                        = 7 + lettercount (div x 100)
                | x > 900                               = 14 + lettercount (mod x 100)
                | x > 700                               = 15 + lettercount (mod x 100)
                | x > 600                               = 13 + lettercount (mod x 100)
                | x > 400                               = 14 + lettercount (mod x 100)
                | x > 300                               = 15 + lettercount (mod x 100)
                | x > 100                               = 13 + lettercount (mod x 100)
                | x > 80                                = 6 + lettercount (mod x 10)
                | x > 70                                = 7 + lettercount (mod x 10)
                | x > 40                                = 5 + lettercount (mod x 10)
                | x > 20                                = 6 + lettercount (mod x 10)
lettercount _ = error "Error: Should not happen"


solve :: IO ()
solve = do
    let x = foldl (\x y -> lettercount y + x) 0 [1..1000]
    let y = assert ((lettercount 977) == 26) "Passed"
    let y2 = assert ((lettercount 89) == 10) "Passed"
    let y3 = assert ((lettercount 123) == 24) "Passed"
    let y4 = assert ((lettercount 90) == 6) "Passed"
    let y5 = assert ((lettercount 100) == 10) "Passed"
    let y6 = assert ((lettercount 200) == 10) "Passed"
    let y7 = assert ((lettercount 300) == 12) "Passed"
    let y8 = assert ((lettercount 400) == 11) "Passed"
    let y8 = assert ((lettercount 500) == 11) "Passed"
    let y9 = assert ((lettercount 600) == 10) "Passed"
    let y10 = assert ((lettercount 700) == 12) "Passed"
    let y11 = assert ((lettercount 800) == 12) "Passed"
    let y12 = assert ((lettercount 900) == 11) "Passed"
    let y13 = assert ((lettercount 115) == 20) "Passed"
    let y14 = assert ((lettercount 342) == 23) "Passed"
    let y15 = assert ((lettercount 708) == 20) "passed"
    let y16 = assert ((lettercount 999) == 24) "passed"
    let y17 = assert ((lettercount 910) == 17) "passed"
    let y18 = assert ((lettercount 41) == 8) "Passed"

    let y19 = assert ((lettercount 11) == 6) "Passed"
    let y20 = assert ((lettercount 20) == 6) "Passed"
    let y21 = assert ((lettercount 30) == 6) "Passed"
    let y22 = assert ((lettercount 50) == 5) "Passed"
    let y23 = assert ((lettercount 60) == 5) "Passed"
    let y24 = assert ((lettercount 70) == 7) "Passed"
    let y25 = assert ((lettercount 80) == 6) "Passed"
    let y26 = assert ((lettercount 90) == 6) "Passed"
    let y27 = assert ((lettercount 101) == 16) "Passed"
    putStrLn $ "result: " ++ show x
    putStrLn $ "The result is: " ++ show y
    putStrLn $ "The result is: " ++ show y2
    putStrLn $ "The result is: " ++ show y3
    putStrLn $ "The result is: " ++ show y4
    putStrLn $ "the result is: " ++ show y5
    putStrLn $ "the result is: " ++ show y6
    putStrLn $ "the result is: " ++ show y7
    putStrLn $ "the result is: " ++ show y8
    putStrLn $ "the result is: " ++ show y9
    putStrLn $ "the result is: " ++ show y10
    putStrLn $ "the result is: " ++ show y11
    putStrLn $ "the result is: " ++ show y12
    putStrLn $ "the result is: " ++ show y13
    putStrLn $ "the result is: " ++ show y14
    putStrLn $ "the result is: " ++ show y15
    putStrLn $ "the result is: " ++ show y16
    putStrLn $ "the result is: " ++ show y17
    putStrLn $ "the result is: " ++ show y18
    putStrLn $ "the result is: " ++ show y19
    putStrLn $ "the result is: " ++ show y20
    putStrLn $ "the result is: " ++ show y21
    putStrLn $ "the result is: " ++ show y22
    putStrLn $ "the result is: " ++ show y23
    putStrLn $ "the result is: " ++ show y24
    putStrLn $ "the result is: " ++ show y25
    putStrLn $ "the result is: " ++ show y26
    putStrLn $ "the result is: " ++ show y27


