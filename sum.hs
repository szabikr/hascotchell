sum' :: [Int] -> Int
sum' (x:y) = x + sum' y
sum' _ = 0