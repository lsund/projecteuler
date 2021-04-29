
module  Problem044 where

import Data.List.Ordered
import Data.List
import Data.Maybe

nthPentagonal :: Int -> Int
nthPentagonal n = n * (3 * n - 1) `div` 2

pentagonals :: [Int]
pentagonals = map nthPentagonal [1..]

isPentagonal :: Int -> Bool
isPentagonal n = member n pentagonals

zipWithShift :: Num a => (a -> a -> a) -> Int -> [a] -> [a]
zipWithShift f n xs = zipWith f (drop n xs) xs

pentagonalDifferences :: Int -> [Int]
pentagonalDifferences n = zipWithShift (-) n pentagonals

pentagonalDifferences' :: Int -> [(Int, Int)]
pentagonalDifferences' n
    | n <= 0    = error "n must be positive"
    | otherwise = zip (pentagonalDifferences n) (repeat n)

pentagonalSums :: Int -> [Int]
pentagonalSums n = zipWithShift (+) n pentagonals

pentagonalSums' :: Int -> [(Int, Int)]
pentagonalSums' n = zip (pentagonalSums n) (repeat n)

allPentagonalDifferences :: [[(Int, Int)]]
allPentagonalDifferences = map pentagonalDifferences' [1..]

allPentagonalDifferencesMerged :: [(Int, Int)]
allPentagonalDifferencesMerged = mergeAll allPentagonalDifferences

indexOfDifference :: (Int, Int) -> Maybe Int
indexOfDifference p@(d, o) = elemIndex p $ pentagonalDifferences' o

sumCorrespondingToDifference :: (Int, Int) -> Int
sumCorrespondingToDifference (d, o) =
    fst $ pentagonalSums' o !! fromJust (indexOfDifference (d, o))

pentagonalPairWithSmallDifference :: Maybe (Int, Int)
pentagonalPairWithSmallDifference =
    let isPentagonalPair (d, o) =
            isPentagonal d && isPentagonal (sumCorrespondingToDifference (d, o))
    in find isPentagonalPair allPentagonalDifferencesMerged

solve :: IO ()
solve = print pentagonalPairWithSmallDifference

