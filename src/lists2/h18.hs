-- (**) Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

slice :: [a] -> Int -> Int -> [a]
slice list start end = foldl helper [] $ zip [1..] list
    where
        helper ac (i,x) =
            if i >= start && i <= end
                then ac ++ [x]
                else ac
