poly :: [Int] -> Int -> Int
poly [] v = 0
poly (x:xs) v | length (x:xs) == 1 = x
              | otherwise = x * (v ^ (length (x:xs) - 1)) + poly xs v 