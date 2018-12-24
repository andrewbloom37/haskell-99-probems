-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

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

data CompressedList a = Single a | Multiple (Int, a) deriving (Show)

compressListData :: [a] -> CompressedList a
compressListData list =
    case list of
        []   -> error "Cannot compress an empty list"
        [x]  -> Single x
        list -> Multiple (length list, head list)

encodeDuplicatesList :: (Eq a) => [a] -> [CompressedList a]
encodeDuplicatesList list = map compressListData $ packList list
