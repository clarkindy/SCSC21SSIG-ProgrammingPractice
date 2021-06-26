module RBTree where

data NodeColor = Black | Red deriving Eq

data RBTree a = Nil
              | Node a NodeColor (RBTree a) (RBTree a) (RBTree a)
              deriving Eq

-- search functions

search :: Ord a => a -> RBTree a -> RBTree a
search x Nil = Nil
search x (Node y c left right parent)
    | x == y    = Node y c left right parent
    | x < y     = search x left
    | otherwise = search x right

contains :: Ord a => a -> RBTree a -> Bool
contains x tree = search x tree /= Nil

-- insertion function

naiveInsert :: Ord a => a -> RBTree a -> RBTree a
naiveInsert x Nil = Node x Black Nil Nil Nil
naiveInsert x (Node y c left right parent)
    | x == y    = Nil
    | x < y     = case left  of Nil   -> Node x Red Nil Nil (Node y c left right parent)
                                left  -> naiveInsert x left
    | otherwise = case right of Nil   -> Node x Red Nil Nil (Node y c left right parent)
                                right -> naiveInsert x right

-- todo : implement case-by-case insertion

repairInsert :: RBTree a -> RBTree a
repairInsert Nil = Nil
repairInsert (Node x c left right parent)

-- deletion function