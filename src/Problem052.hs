module Problem052 where

import Util

containDigits x = do
    print $ int2List x
    print $ int2List (2 * x)
    print $ elements (int2List x) (int2List (2 * x))
