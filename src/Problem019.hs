module Problem019 where

-- define monday as 1

weekdayCode :: Int -> Int -> Int
weekdayCode start n = mod (start + n) 7

weekday :: Int -> Int -> String
weekday start n =
    let code = weekdayCode start n
    in case code of 0 -> "Monday"
                    1 -> "Tuesday"
                    2 -> "Wednesday"
                    3 -> "Thursday"
                    4 -> "Friday"
                    5 -> "Saturday"
                    6 -> "Sunday"

numberOfDays :: Int -> Int -> Int -> Int
numberOfDays from to offset =
    let nyears = to - from
    in  nyears * 365 + div nyears 4 + offset

