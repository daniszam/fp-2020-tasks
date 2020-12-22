module Part2 where

import Part2.Types

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
prob7 color = getInt >= 0 && getInt <= 255
      where
        getInt = prob9 color

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color part = case part of
  Red x -> color {red = red color + x}
  Green x -> color {green = green color + x}
  Blue x -> color {blue = blue color + x}

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 color = case color of
  Red x -> x
  Green x -> x
  Blue x -> x

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 color
  | red color > green color && red color > blue color = Just (Red (red color))
  | green color > blue color && green color > red color = Just (Green (green color))
  | blue color > green color && blue color > red color = Just (Blue (blue color))
  | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
--TODO неправильный тест ??? Tree
-- {left = Just (Tree {left = Nothing, root = 1, right = Nothing}),
-- root = 3, right = Just (Tree {left = Just (Tree {left = Nothing, root = 5, right = Nothing}),
-- root = 6, right = Nothing})}
-- почему-то в тесте указано (1 2 (3 4 (5 6 nil)))
prob11 :: Num a => Tree a -> a
prob11 tree = sum (toList tree)

toList :: Tree a -> [a]
toList tree = maybeToList (left tree) ++ [root tree] ++ maybeToList (right tree)
  where
    maybeToList (Just x) = toList x
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
prob12 tree = checkRight (right tree) (root tree) && checkLeft (left tree) (root tree)

checkRight :: Ord a => Maybe (Tree a) -> a -> Bool
checkRight Nothing x = True
checkRight (Just tree) parent = root tree >= parent && checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)

checkLeft :: Ord a => Maybe (Tree a) -> a -> Bool
checkLeft Nothing x = True
checkLeft (Just tree) parent = root tree < parent && checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 a tree = hasValue a (Just tree)

hasValue :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
hasValue a Nothing = Nothing
hasValue a (Just tree)
  | a > root tree = hasValue a (right tree)
  | a < root tree = hasValue a (left tree)
  | otherwise = Just tree

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 tree = changeToInt tree 1

changeToInt :: Tree () -> Int -> Tree Int
changeToInt tree currentNum = Tree {left = leftTree, root = num, right = rightTree}
  where
    rightTree = getTree (right tree) currentNum
    num = getRootNum rightTree
      where
        getRootNum :: Maybe (Tree Int) -> Int
        getRootNum Nothing = currentNum
        getRootNum (Just tree) = root tree + 1
    leftTree = getTree (left tree) (num + 1)

getTree :: Maybe (Tree ()) -> Int -> Maybe (Tree Int)
getTree Nothing x = Nothing
getTree (Just tree) currentNum = Just (Tree {left = leftTree, root = num, right = rightTree})
  where
    rightTree = getTree (right tree) currentNum
    num = getRootNum rightTree
      where
        getRootNum :: Maybe (Tree Int) -> Int
        getRootNum Nothing = currentNum
        getRootNum (Just tree) = root tree + 1
    leftTree = getTree (left tree) (num + 1)

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = Tree {left = leftTree, root = num, right = rightTree}
  where
    q = getRight (right tree)
      where
        getRight :: Maybe (Tree a) -> Tree a
        getRight Nothing = error "No right tree"
        getRight (Just treeQ) = treeQ

    rightTreeB = left q
    leftTree = Just (Tree {left = left tree, root = root tree, right = rightTreeB})
    num = root q
    rightTree = right q

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = Tree {left = leftTree, root = num, right = rightTree}
  where
    p = getLeft (left tree)
      where
        getLeft :: Maybe (Tree a) -> Tree a
        getLeft Nothing = error "No left tree"
        getLeft (Just treeP) = treeP

    rightTreeB = right p
    rightTree = Just (Tree {left = rightTreeB, root = root tree, right = right tree})
    num = root p
    leftTree = left p

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 tree = case buildBalanced (toList tree) of
                   Just a -> a
                   Nothing -> tree
 
buildBalanced :: [a] -> Maybe (Tree a)
buildBalanced [] = Nothing
buildBalanced elts =
  Just (Tree
    (buildBalanced $ take half elts)
    (elts !! half)
    (buildBalanced $ drop (half + 1) elts))
  where
    half = length elts `quot` 2
       
