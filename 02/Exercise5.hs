import Set

-- a
complete :: (Ord a) => a -> Int -> Tree a
complete x 0 = E
complete x d = T t x t
    where t = complete x (d-1)

