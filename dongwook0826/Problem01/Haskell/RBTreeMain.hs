import RBTree (RBTree, contains, insert, delete, genRBTree)

myList :: [Integer]
myList = [15, 22, 6, 1, 17, 27, 15, 13, 8, 25, 11, 17]

mySearchList :: [Integer]
mySearchList = [8, 12, 17, 9, 25, 2, 15]

myDelList :: [Integer]
myDelList = [1, 11, 15, 12, 27, 13, 2, 17, 9, 17, 22, 15, 8, 6, 25]

myTree :: RBTree Integer
myTree = genRBTree myList

main :: IO ()
main = do
    print myTree
    print $ map (`contains` myTree) mySearchList
    print $ foldr delete myTree myDelList
    putStrLn "all deleted, re-generating..."
    print $ genRBTree myDelList

{- result
 17 (Black)
- 11 (Red)
-- 6 (Black)
--- 1 (Red)
--+ 8 (Red)
-+ 15 (Black)
-+- 13 (Red)
-++ 15 (Red)
+ 25 (Red)
+- 17 (Black)
+-+ 22 (Red)
++ 27 (Black)

[True,False,True,False,True,False,True]

all deleted, re-generating...
 15 (Black)
- 8 (Black)
-- 2 (Black)
--- 1 (Red)
--+ 6 (Red)
-+ 12 (Red)
-+- 9 (Black)
-+-+ 11 (Red)
-++ 13 (Black)
+ 22 (Black)
+- 17 (Black)
+-- 15 (Red)
+-+ 17 (Red)
++ 25 (Black)
+++ 27 (Red)
-}