-- (*) Reverse a list.

helper :: [a] -> [a] -> [a]
helper [] stack = stack
helper (x:xs) stack = helper xs $ x : stack

listReverse :: [a] -> [a]
listReverse list =
    case list of
        []    -> []
        [x]   -> [x]
        [x,y] -> [y,x]
        list  -> helper list []
