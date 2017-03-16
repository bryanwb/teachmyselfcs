lastButOne :: [a] -> a
lastButOne [x] = x
lastButOne xs = if length xs == 2
                then head xs
                else lastButOne (tail xs)
