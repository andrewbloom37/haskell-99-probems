-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

helper :: (Eq a) => [a] -> a -> [a] -> [[a]] -> [[a]]
helper [] _ currentList outputList = outputList ++ [currentList]
helper (x:xs) currentVal currentList outputList
    | x == currentVal = helper xs x (x : currentList) outputList
    | otherwise       = helper xs x [x] (outputList ++ [currentList])

packList :: (Eq a) => [a] -> [[a]]
packList list =
    case list of
        []     -> []
        (x:xs) -> helper xs x [x] []
