-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

data CompressedList a = Single a | Multiple (Int, a) deriving (Show)

helper :: (Eq a) => [a] -> a -> CompressedList a -> [CompressedList a] -> [CompressedList a]
helper [] _ currentData outputList = outputList ++ [currentData]
helper (x:xs) currentVal (Single z) outputList
    | x == currentVal = helper xs x (Multiple (2, z)) outputList
    | otherwise       = helper xs x (Single x) (outputList ++ [Single z])
helper (x:xs) currentVal (Multiple (y, z)) outputList
    | x == currentVal = helper xs x (Multiple (y + 1, z)) outputList
    | otherwise       = helper xs x (Single x) (outputList ++ [Multiple (y, z)])

packList :: (Eq a) => [a] -> [CompressedList a]
packList list =
    case list of
        []     -> []
        (x:xs) -> helper xs x (Single x) []
