module RedBlackTree where

data Color = Red | Black deriving (Show, Eq)

data Tree a = Empty | Tree Color a (Tree a) (Tree a)
            deriving Show

