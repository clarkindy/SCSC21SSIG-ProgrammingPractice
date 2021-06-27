module RBTree
( RBTree
, contains
, insert
, delete
, genRBTree
) where

data NodeColor = Black | Red deriving (Eq, Show)

data RBTree a = Nil
              | Node a NodeColor (RBTree a) (RBTree a)
              deriving Eq

showWithDepth :: Show a => RBTree a -> String -> String
showWithDepth Nil _ = ""
showWithDepth (Node e c left right) dep =
    dep ++ " " ++ show e ++ " (" ++ show c ++ ")\n"
        ++ showWithDepth left (dep ++ "-")
        ++ showWithDepth right (dep ++ "+")

instance (Show a) => Show (RBTree a) where
    show tree = showWithDepth tree ""

-- private util const's & ftn's

colorOf :: RBTree a -> NodeColor
colorOf Nil = Black
colorOf (Node _ c _ _) = c

valueOf :: RBTree a -> Maybe a
valueOf Nil = Nothing
valueOf (Node e _ _ _) = Just e

setBlack :: RBTree a -> RBTree a
setBlack Nil = Nil
setBlack (Node e _ left right) = Node e Black left right

setRed :: RBTree a -> RBTree a
setRed Nil = Nil
setRed (Node e _ left right) = Node e Red left right

type Direction = Bool

toRight :: Bool
toRight = True

toLeft :: Bool
toLeft = False

rotate :: RBTree a -> Direction -> RBTree a
rotate (Node e c (Node le lc ll lr) r) toRight
    = Node le lc ll (Node e c lr r)
rotate (Node e c l (Node re rc rl rr)) toLeft
    = Node re rc (Node e c l rl) rr
rotate tree _ = tree

-- search

contains :: Ord a => a -> RBTree a -> Bool
contains x Nil = False
contains x (Node e c left right)
    | x == e    = True
    | x < e     = contains x left
    | otherwise = contains x right

-- insert

insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = setBlack $ insert' x tree

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' x Nil = Node x Red Nil Nil
insert' x (Node e c left right)
    | x < e     = modifyInsert $ Node e c (insert' x left) right
    | otherwise = modifyInsert $ Node e c left (insert' x right)

modifyInsert :: RBTree a -> RBTree a
modifyInsert (Node g Black
    (Node p Red (Node e Red left right) pright)
    uncleTree)
    | colorOf uncleTree == Red =
        Node g Red
            (Node p Black (Node e Red left right) pright)
            (setBlack uncleTree)
    | otherwise =
        Node p Black
            (Node e Red left right)
            (Node g Red pright uncleTree)
modifyInsert (Node g Black
    (Node p Red pleft (Node e Red left right))
    uncleTree)
    | colorOf uncleTree == Red =
        Node g Red
            (Node p Black pleft (Node e Red left right))
            (setBlack uncleTree)
    | otherwise =
        modifyInsert (Node g Black
            (rotate (Node p Red pleft (Node e Red left right)) toLeft)
            uncleTree)
modifyInsert (Node g Black
    uncleTree
    (Node p Red (Node e Red left right) pright))
    | colorOf uncleTree == Red =
        Node g Red
            (setBlack uncleTree)
            (Node p Black (Node e Red left right) pright)
    | otherwise =
        modifyInsert (Node g Black
            uncleTree
            (rotate (Node p Red (Node e Red left right) pright) toRight))
modifyInsert (Node g Black
    uncleTree
    (Node p Red pleft (Node e Red left right)))
    | colorOf uncleTree == Red =
        Node g Red
            (setBlack uncleTree)
            (Node p Black pleft (Node e Red left right))
    | otherwise =
        Node p Black
            (Node g Red uncleTree pleft)
            (Node e Red left right)
modifyInsert tree = tree

genRBTree :: Ord a => [a] -> RBTree a
genRBTree = foldr insert Nil

-- delete
-- todo: implement deleting functions

delete :: Ord a => a -> RBTree a -> RBTree a
delete _ Nil = Nil
delete x (Node e c Nil Nil) -- root
    | x == e    = Nil
    | otherwise = Node e c Nil Nil
delete x tree = fst $ delete' x tree

delete' :: Ord a => a -> RBTree a -> (RBTree a, Bool)
delete' _ Nil = (Nil, True)
delete' x (Node e c left right)
    | x < e     = let
        (deletedTree, wellDeleted) = delete' x left
        (modifiedTree, allModified) = modifyDelete wellDeleted (Node e c deletedTree right) toLeft in 
            (modifiedTree, wellDeleted || allModified)
    | x > e     = let
        (deletedTree, wellDeleted) = delete' x right
        (modifiedTree, allModified) = modifyDelete wellDeleted (Node e c left deletedTree) toRight in
            (modifiedTree, wellDeleted || allModified)
    | otherwise {- x == e -} = case (c, left, right) of
        (_, Nil, Nil) -> (Nil, c == Red) -- if Black, modification needed
        (Black, Nil, Node y _ _ _) -> (Node y Black Nil Nil, True)
        (Black, Node y _ _ _, Nil) -> (Node y Black Nil Nil, True)
        (_, _, _) -> let
            (delMinRight, Just min) = deleteMin right
            deleteMin (Node e c left right)
                | left == Nil = (right, Just e)
                | otherwise = let (delLeft, leftmost) = deleteMin left in
                    (Node e c delLeft right, leftmost)
            deleteMin Nil = (Nil, Nothing) in
                (Node min c left delMinRight, True)

modifyDelete :: (Ord a) => Bool -> RBTree a -> Direction -> (RBTree a, Bool)
modifyDelete True tree _ = (tree, True)
modifyDelete False (Node e c left (Node s sc sleft sright)) toLeft =
    case (c, sc, colorOf sleft, colorOf sright) of
        (Black, Black, Black, Black) ->
            (Node e c left (Node s Red sleft sright), False)
        (Black, Red, _, _) ->
            (Node s Black (Node e Black left $ setRed sleft) sright, True)
        (Red, _, Black, Black) ->
            (Node e Black left (fst $ modifyDelete False (Node s Red sleft sright) toLeft), True)
        (_, Black, Red, Black) -> let Node n nc nleft nright = sleft in
            (Node n c (Node e Black left nleft) (Node s Black nright sright), True)
        (_, Black, _, Red) ->
            (Node s c (Node e Black left sleft) $ setBlack sright, True)
        (_, _, _, _) -> error "unexpected coloring case..."
modifyDelete False (Node e c (Node s sc sleft sright) right) toRight =
    case (c, sc, colorOf sleft, colorOf sright) of
        (Black, Black, Black, Black) ->
            (Node e c (Node s Red sleft sright) right, False)
        (Black, Red, _, _) ->
            (Node s Black sleft (Node e Black right $ setRed sright), True)
        (Red, _, Black, Black) ->
            (Node e Black (fst $ modifyDelete False (Node s Red sleft sright) toRight) right, True)
        (_, Black, Black, Red) -> let Node n nc nleft nright = sright in
            (Node n c (Node s Black sleft nleft) (Node e Black nright right), True)
        (_, Black, Red, _) ->
            (Node s c (setBlack sleft) $ Node e Black sright right, True)
        (_, _, _, _) -> error "unexpected coloring case..."
modifyDelete False tree _ = (tree, True)
