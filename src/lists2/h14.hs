--(*) Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli = foldl (\x y -> x ++ [y, y]) []
