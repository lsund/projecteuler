module Problem022 where

import System.IO
import Control.Monad
import Data.List.Split
import Data.List
import Data.Maybe

alphapos:: Int -> [Char] -> [[Char]] -> Int
alphapos pos s (x : xs) | s == x = pos
                        | otherwise = alphapos (pos + 1) s xs

alphaval :: [Char] -> Int
alphaval [] = 0
alphaval (c : cs)   | c == '\n' = 0
                    | otherwise  = fromJust (elemIndex c ['A'..'Z']) + 1 + alphaval cs

score :: [Char] -> [[Char]] -> Int
score cs namelist = alphapos 1 cs namelist * alphaval cs

totalscore :: [[Char]] -> Int -> Int
totalscore [] _ = 0
totalscore (s : ss) pos = (alphaval s * pos) + totalscore ss (pos + 1)

solve :: IO ()
solve =  do
        contents <- readFile "p022_names.txt"
        let sorted = sort $ splitOn " " contents
        let x = totalscore sorted 1
        print contents
        print x


