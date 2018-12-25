-- (*) Split a list into two parts; the length of the first part is given.

-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a],[a])
split list ind =
    helper list ind [] []
        where
            helper [] _ first second       = (first,second)
            helper list 0 first second     = helper [] (-1) first (second ++ list)
            helper (x:xs) ind first second = helper xs (ind - 1) (first ++ [x]) second
