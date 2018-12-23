--(*) Find the K'th element of a list. The first element in the list is number 1.

-- kthEl :: [a] -> Int -> a
-- kthEl list k =
--     if length list < k
--         then error "The list does not have that many elements"
--         else if k < 1
--             then error "k must be non-zero and non-negative"
--             else list !! (k - 1)

-- Without using length
kthEl :: [a] -> Int -> a
kthEl [] _      = error "Invalid input"
kthEl (x:_) 1   = x
kthEl (_:xs) k
    | k < 1     = error "k must be non-zero and non-negative"
    | otherwise = kthEl xs (k - 1)
