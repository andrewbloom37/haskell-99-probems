-- (*) Reverse a list.

-- foldl is similar to JavaScript's reduce, it takes a function and an initial
-- value, and applies the output of that as the input to the next iteration.
-- flip switches the order of the arguments being applied.
listReverse :: [a] -> [a]
listReverse list =
    case list of
        []    -> []
        [x]   -> [x]
        list  -> foldl (flip (:)) [] list
