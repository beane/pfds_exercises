import Set

member' :: Ord a => a -> Tree a -> Bool
member' _ E = False
member' x t@(T _ y _) = memberWithLastValue x y t

memberWithLastValue :: Ord a => a -> a -> Tree a -> Bool
memberWithLastValue x y E = x == y
memberWithLastValue x y (T left z right)
    | x < z     = memberWithLastValue x y left
    | otherwise = memberWithLastValue x z right
