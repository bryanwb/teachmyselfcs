import Data.List
import Data.Either

nelems :: [t] -> Int
nelems [] = 0
nelems [x] = 1
nelems (x:xs) = 1 + nelems xs

mymean :: [Int] -> Float
mymean [] = 0
mymean x = (fromIntegral (foldl (+) 0 x)) / fromIntegral (length x)

ispalindrome :: (Eq a) => [a] -> Bool
ispalindrome [] = True
ispalindrome [x] = True
ispalindrome x = x == reverse x

sortLists :: (Ord a) => [[a]] -> [[a]]
sortLists l = sortBy (\x y -> compare (length x) (length y)) l


myintersperse :: a -> [[a]] -> [a]
myintersperse sep [x] = x
myintersperse sep [] = []
myintersperse sep (x:xs) = x ++ [sep] ++ (myintersperse sep xs)


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node a Empty Empty) = 1
treeHeight (Node a b c) = 1 + max (treeHeight b) (treeHeight c)

-- let simpleTree = Node "foo" (Node "bar" (Node "quux" Empty Empty) (Node "qarx" Empty Empty)) (Node "barx" (Node "boox" Empty Empty) (Node "qeez" Empty Empty))

simpleTree = Node "parent" (Node "left" Empty Empty)
                           (Node "right" (Node "2ndright" Empty Empty) (Node "2ndright" Empty Empty))

