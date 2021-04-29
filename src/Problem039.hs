module Problem039 where

pythogareanTriangles :: Int -> [(Int, Int, Int)]
pythogareanTriangles p =
    [(a, b, c) |
        a <- [1..(p `div` 2)],
        b <- [a..(p - a)],
        c <- [b..(p - a - b)],
        a + b + c == p,
        a^2 + b^2 == c^2]

npythogareanTriangles = length . pythogareanTriangles

lower = 400
upper = 500

candidates :: [Int]
candidates = [lower, (lower + 2)..upper]

trianglesListed = map npythogareanTriangles candidates

-- answer 840 through identifying that 120, 240, 420 and 840 had many triangle among
-- similar numbers
