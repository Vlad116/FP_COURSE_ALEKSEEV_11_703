module Part2 where

import Part2.Types

import Data.Function ((&))
------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'
------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 colorPart = getIntFromColorPart >= 0 && getIntFromColorPart <= 255
    where getIntFromColorPart = prob9 colorPart
------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color colorPart = case colorPart of
  Red x -> color {red = red color + x}
  Green x -> color {green = green color + x}
  Blue x -> color {blue = blue color + x}

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 colorPart = case colorPart of
    Red int   -> int
    Green int -> int
    Blue int  -> int
------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
colorPartToInt :: ColorPart -> Int
colorPartToInt color = prob9 color

maxBy :: Ord a => (t -> a) -> [t] -> t
maxBy f (x:xs) = iter x xs
  where
    iter acc [] = acc
    iter acc (x:xs) = if f acc < f x then iter x xs else iter acc xs

decompose :: Color -> [ColorPart]
decompose (Color r g b) = [Red r, Green g, Blue b]

prob10 :: Color -> Maybe ColorPart
prob10 color = if length maxs == 1 then Just maxValue else Nothing
  where
    colors = decompose color
    maxValue = maxBy colorPartToInt colors
    maxs = filter (== colorPartToInt maxValue) (map colorPartToInt colors)
------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = sum (treeToList tree)

treeToList :: Tree a -> [a]
treeToList tree = maybeToList (left tree) ++ [root tree] ++ maybeToList (right tree)
  where
    maybeToList (Just x) = treeToList x
    maybeToList Nothing = []
------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 tree = checkTree tree
  where
    checkTree :: Ord a => Tree a -> Bool
    checkTree tree = checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)
      where
        checkRight :: Ord a => Maybe (Tree a) -> a -> Bool
        checkRight Nothing x = True
        checkRight (Just tree) parent = root tree >= parent && checkTree tree

        checkLeft :: Ord a => Maybe (Tree a) -> a -> Bool
        checkLeft Nothing x = True
        checkLeft (Just tree) parent = root tree < parent && checkTree tree
 

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 val tree = findTree val (Just tree)
  where
    findTree :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
    findTree _ Nothing = Nothing
    findTree val (Just tree)
      | val > root tree = findTree val (right tree)
      | val < root tree = findTree val (left tree)
      | otherwise = Just tree

findTree :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
findTree a Nothing = Nothing
findTree a (Just tree)
  | a > root tree = findTree a (right tree)
  | a < root tree = findTree a (left tree)
  | otherwise = Just tree

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 unitTree = traverseTree (getNodesCount unitTree) unitTree
    where
        traverseTree :: Int -> Tree () -> Tree Int
        traverseTree nodeNumber tree = Tree
            (do
                leftSubTree <- tree & left
                return $ traverseTree (pred nodeNumber) leftSubTree)
            nodeNumber
            (do
                rightSubTree <- tree & right
                return $ traverseTree (getRightDecrementFunc tree nodeNumber) rightSubTree)

        getRightDecrementFunc :: Tree a -> (Int -> Int)
        getRightDecrementFunc tree = case tree & left of
            Just leftSubTree -> subtract (getNodesCount leftSubTree + 1)
            Nothing -> pred

        getNodesCount :: Tree a -> Int
        getNodesCount tree = succ $ sum
            [
                maybe 0 getNodesCount (tree & left),
                maybe 0 getNodesCount (tree & right)
            ]

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree rotateLeft (right tree)
    where
        rotateLeft q = q { left = Just oldRoot }
           where
               oldRoot = tree { right = left q }

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rotateRight (left tree)
  where
    rotateRight subTree = subTree {right = Just oldTree}
      where
        oldTree = tree {left = right subTree}

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 tree = case buildBalancedTree (treeToList tree) of
                   Just a -> a
                   Nothing -> tree
 
buildBalancedTree :: [a] -> Maybe (Tree a)
buildBalancedTree [] = Nothing
buildBalancedTree elts =
  Just (Tree
    (buildBalancedTree $ take half elts)
    (elts !! half)
    (buildBalancedTree $ drop (half + 1) elts))
  where
    half = length elts `quot` 2 