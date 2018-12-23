--(*) Find the last element of a list.

myLast :: [a] -> a
myLast list =
    case list of
        []     -> error "There is no last element of an empty list"
        [x]    -> x
        (x:xs) -> myLast xs
