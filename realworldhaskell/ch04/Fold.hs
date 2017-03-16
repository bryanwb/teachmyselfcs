import Data.Char (digitToInt)
import Data.List (foldl',tails)

myFilter p xs = foldr step [] xs
  where step x ys | p x = x : ys
                  | otherwise = ys

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr step [] xs
  where step x ys = f x : ys

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = foldr (:) ys xs

asInt_fold :: String -> Int
asInt_fold xs = foldl step 0 xs
  where step x y = x * 10 + digitToInt y

nicerSum :: [Integer] -> Integer
nicerSum = foldl (+) 0

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

suffixes2 = init . tails


