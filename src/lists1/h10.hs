--(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

helper :: (Eq a) => [a] -> a -> [a] -> [[a]] -> [[a]]
helper [] _ currentList outputList = outputList ++ [currentList]
helper (x:xs) currentVal currentList outputList
    | x == currentVal = helper xs x (x : currentList) outputList
    | otherwise       = helper xs x [x] (outputList ++ [currentList])

packList :: (Eq a) => [a] -> [[a]]
packList list =
    case list of
        []     -> []
        [x]    -> [[x]]
        (x:xs) -> helper xs x [x] []

compressListData :: [a] -> (Int, a)
compressListData [] = error "Cannot compress an empty list"
compressListData list = (length list, head list)

encodeDuplicatesList :: (Eq a) => [a] -> [(Int, a)]
encodeDuplicatesList list = map compressListData $ packList list
