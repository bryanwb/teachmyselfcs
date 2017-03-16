data Color = Red | Green | Blue

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"


instance Read Color where
  readsPrec _ value =
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where tryParse [] = []
          tryParse ((attempt, result):xs) =
            if (take (length attempt) value) == attempt
               then [(result, drop (length attempt) value)]
               else tryParse xs
    
