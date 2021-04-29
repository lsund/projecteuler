module Problem028 where

diagonalLength dim = div dim 2


numberAtTR i = 4 * i ^ 2 - 4 * i + 1

numberAtTL i = 4 * i ^ 2 - 6 * i + 3

numberAtBR i = 4 * i ^ 2 - 10 * i + 7

numberAtBL i = 4 * i ^ 2 - 8 * i + 5


indexes = [2..diagonalLength 1001 + 1]

trsum = sum $ map numberAtTR indexes
tlsum = sum $ map numberAtTL indexes
brsum = sum $ map numberAtBR indexes
blsum = sum $ map numberAtBL indexes

total = sum [trsum, tlsum, brsum, blsum] + 1

