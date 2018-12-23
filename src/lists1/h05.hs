--(*) Find the number of elements of a list.

customLength :: [a] -> Int
customLength list =
    case list of
        []     -> 0
        [x]    -> 1
        (_:xs) -> 1 + customLength xs
