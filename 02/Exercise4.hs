import Set

insert'' :: (Ord a) => a -> Tree a -> Tree a
insert'' x E = T E x E
insert'' x t@(T left y right) = insertWithLastValue x y t

insertWithLastValue :: (Ord a) => a -> a -> Tree a -> Tree a
insertWithLastValue x y E
    | x == y    = error "element already exists"
    | otherwise = T E x E
insertWithLastValue x y (T left z right)
    | x < z     = T (insertWithLastValue x y left) z right
    | otherwise = T left z (insertWithLastValue x z right)
