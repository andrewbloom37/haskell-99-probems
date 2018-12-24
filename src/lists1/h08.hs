-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

-- This is an ideal situation for using a set, however in the spirit of the
-- problem, I have avoided doing so.
-- TODO: see about optimizing this solution; the list concat is not ideal
helper :: (Eq a) => [a] -> [a] -> [a]
helper [] set = set
helper (x:xs) set =
    if x `notElem` set
        then helper xs $ set ++ [x]
        else helper xs set

compressList :: (Eq a) => [a] -> [a]
compressList list = helper list []
