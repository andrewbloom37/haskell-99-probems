-- Insert an element at a given position into a list.

insert :: a -> [a] -> Int -> [a]
insert el list loc = reverse $ helper loc list []
    where
        helper _ [] out       = out
        helper 1 (x:xs) out   = helper 0 xs (x:el:out)
        helper num (x:xs) out = helper (num - 1) xs (x:out)
