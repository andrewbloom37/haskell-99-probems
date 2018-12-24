-- (**) Flatten a nested list structure.

-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

data NestedList a = Elem a | List [NestedList a]

-- Note: due to Haskell's monomorphism restriction, we must use the List type
--       constructor below in the recursive pattern match, simply stating
--       `flattenList xs` will cause a compiler error, because it needs to be
--       explicitly stated that the xs is a List to be a valid member of the
--       NestedList type in order to be passed in as a parameter to flattenList 
flattenList :: NestedList a -> [a]
flattenList list =
    case list of
        (Elem x)      -> [x]
        (List [])     -> []
        (List (x:xs)) -> flattenList x ++ flattenList (List xs)
