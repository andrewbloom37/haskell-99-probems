--(*) Find the last but one element of a list.

lastButOne :: [a] -> a
lastButOne list =
    case list of
        []     -> error "There is no second-to-last element of an empty list"
        [x]    -> error "There is no second-to-last element of a list of length one"
        [x,y]  -> x
        (x:xs) -> lastButOne xs
