
module Problem047 where

import Data.List.Ordered
import qualified Data.List

primes :: [Int]
primes = eratos [2..]
    where
        eratos []     = []
        eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])

factor :: Int -> [Int]
factor = factor' primes
    where
        factor' _ 1          = []
        factor' (p : ps) n
            | n `mod` p == 0 = p : factor' (p : ps) (n `div` p)
            | otherwise      = factor' ps n

distinctFactorLengths :: [Int] -> [Int]
distinctFactorLengths = map (length . nub . factor)

atLeastNDistinctFactors :: Int -> [Int] -> Bool
atLeastNDistinctFactors n = all (>= n) . distinctFactorLengths

consecutives :: (Enum t, Num t) => t -> [[t]]
consecutives n = [[x..x + n - 1] | x <- [1..]]

solution :: Maybe [Int]
solution = Data.List.find (atLeastNDistinctFactors 4) (consecutives 4)


