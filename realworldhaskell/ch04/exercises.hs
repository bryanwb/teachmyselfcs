safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead [x] = Just x
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [x] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = (:) <$> pure x <*> safeInit xs

-- splitWith :: (a -> Bool) -> [a] -> [[a]]
-- splitWith _ [] = []
-- splitWith pred l =
--   case break pred l of
--     ([],[]) -> []
--     (a, []) -> [a]
--     ([], b) -> case b of
--       (x:[]) -> splitWith pred [x]
--       (x:xs) -> x : (splitWith pred xs)
--     (a, b) -> a : (splitWith pred (tail b))
-- splitWith f xs = case xs of
--                    [] -> []
--                    (x:xs) -> case (f x) of
--                                True -> splitWith f xs
--                                False -> [x] : (splitWith f xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p l =
  let (pre, suf) = span (not . p) l
  in case (pre, suf) of
    ([], b) -> splitWith p b
    (a, []) -> [a]
    (a, b) -> [a] ++ splitWith p b
