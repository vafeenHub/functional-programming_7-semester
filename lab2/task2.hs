-- 8. Дано бинарное дерево, найти сумму элементов, имеющих два потомка. 


data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Eq)

sumWithTwoChildren :: Tree Int->Int
sumWithTwoChildren Empty = 0
sumWithTwoChildren (Node x left right) =
  let
    leftSum  = sumWithTwoChildren left
    rightSum = sumWithTwoChildren right
    current  = if left /= Empty && right /= Empty then x else 0
  in
    current + leftSum + rightSum


-- Пример дерева:
--       5
--      / \
--     3   8
--    /   / \
--   2   7   9
--        \
--         7

exampleTree :: Tree Int
exampleTree =
  Node 5
    (Node 3 (Node 2 Empty Empty) Empty)
    (Node 8
      (Node 7 Empty (Node 7 Empty Empty))
      (Node 9 Empty Empty)
    )
main :: IO ()
main = print $ sumWithTwoChildren exampleTree

