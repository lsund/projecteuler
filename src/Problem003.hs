module Problem003 where

isprime :: Int -> Bool
isprime 2 = True
isprime k = 0 `notElem` (map (mod k) [3..k - 1])

primefactors 1 i = []
primefactors x i =
	if isprime i && mod x i == 0 then
		i : primefactors (div x i) (i + 1)
	else
		primefactors (x) (i + 1)

solution =
    let num = 600851475143
    in primefactors num 2
