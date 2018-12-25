--(**) Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli list times = foldl (\ac v -> ac ++ replicateList times v) [] list
    where
        replicateList 0 _ = []
        replicateList n x = x : replicateList (n - 1) x
