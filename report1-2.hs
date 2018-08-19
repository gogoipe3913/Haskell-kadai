myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) | p x = (x : myTakeWhile p xs)
                     | otherwise = []