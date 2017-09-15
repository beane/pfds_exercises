{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module FiniteMap where

class FiniteMap m k a where
    mapEmpty :: m k a
    bind :: k -> a -> m k a -> m k a
    lookup :: k -> m k a -> a

data Map k v = Map (Map k v) k v (Map k v) | EmptyMap

instance (Ord k) => FiniteMap Map k v where
    mapEmpty = EmptyMap
    bind k v EmptyMap = Map EmptyMap k v EmptyMap
    bind k v (Map left key val right)
        | k < key = Map (bind k v left) key val right
        | otherwise = Map left key val (bind k v right)
    lookup k EmptyMap = error "haha"
    lookup k (Map left key val right)
        | k < key = FiniteMap.lookup k left
        | k > key = FiniteMap.lookup k right
        | otherwise = val
