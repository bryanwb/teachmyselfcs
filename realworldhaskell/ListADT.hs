import Data.Maybe

data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons x xs) = (:) x (toList xs)
toList Nil = []


data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving (Show)

simpleTree = Node "parent" (Just(Node "left child" Nothing Nothing))
             (Just (Node "right child" Nothing Nothing))
