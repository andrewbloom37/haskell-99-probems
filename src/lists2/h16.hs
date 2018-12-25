--(**) Drop every N'th element from a list.

dropNthEl :: [a] -> Int -> [a]
dropNthEl list num = foldl helper [] $ zip [1..] list
    where
        helper ac (i,x) =
            if i `rem` num == 0
                then ac
                else ac ++ [x]
