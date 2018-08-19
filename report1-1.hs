myTake :: Int -> [a] -> [a]
myTake 0 (x:xs) = []
myTake n (x:xs) | length (x:xs) < n = (x:xs) 
                | otherwise = x : myTake (n-1) xs