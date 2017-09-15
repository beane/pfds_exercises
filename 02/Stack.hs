{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Stack where

class Stack s a where
    stackEmpty :: s a
    isEmpty :: s a -> Bool
    cons :: a -> s a -> s a
    head :: s a -> a
    tail :: s a -> s a

instance Stack [] a where
    stackEmpty = []
    isEmpty = (0==) . length
    cons = (:)
    head = Prelude.head
    tail = Prelude.tail

