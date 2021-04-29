module Problem024 where

import Data.List

perm = (sort $ permutations [0..9]) !! 999999
