{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} 
module Set where

class Set s a where
    empty :: s a
    insert :: a -> s a -> s a
    member :: a -> s a -> Bool

data Tree a = E | T (Tree a) a (Tree a) deriving (Show, Eq)

instance (Ord a) => Set Tree a where
    empty = E
    member _ E = False
    member x (T left y right)
        | x < y     = member x left
        | x > y     = member x right
        | otherwise = True
    insert x E = T E x E
    insert x t@(T left y right)
        | x < y     = T (insert x left) y right
        | x > y     = T left y (insert x right)
        | otherwise = t
