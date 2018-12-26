-- (**) Rotate a list N places to the left.

-- Hint: Use the predefined functions length and (++).

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate list 0 = list
rotate (x:xs) places =
    if places > 0
        then rotate (xs ++ [x]) $ places - 1
        else rotate (end:start) $ places + 1
            where
                list = x:xs
                end = list !! (length list - 1)
                start = take (length list - 1) list
