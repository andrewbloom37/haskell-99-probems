-- (*) Remove the K'th element from a list.

remove :: Int -> [a] -> [a]
remove position list = foldl helper [] $ zip [1..] list
    where
        helper ac (i,x) =
            if i /= position
                then ac ++ [x]
                else ac
