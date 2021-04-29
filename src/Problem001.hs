module Problem001 where

sumlist [] = 0
sumlist (x : xs) = x + sumlist xs

listnums [] = []
listnums (x : xs) = if mod x 3 == 0 || mod x 5 == 0 then x : listnums xs else listnums xs

solution = sumlist $ listnums [1..999]
