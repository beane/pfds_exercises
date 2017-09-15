import Set

insert' :: (Ord a) => a -> Tree a -> Tree a
insert' x E = T E x E
insert' x t@(T left y right)
    | x < y     = T (insert' x left) y right
    | x > y     = T left y (insert' x right)
    | otherwise = error "element already exists"

