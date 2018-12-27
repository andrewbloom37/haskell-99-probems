-- Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range start end = helper (end - start) [start]
    where
        helper 0 list   = list
        helper num list =
            if num < 0
                then helper (num + 1) (list ++ [end - num - 1])
                else helper (num - 1) (list ++ [end - num + 1])
