merge3 :: (Eq a, Ord a) => [a] -> [a] -> [a] -> [a]
merge3 [] [] [] = []
merge3 (x:xs) (y:ys) (z:zs) = sortByValue (uniqList ((x:xs) ++ (y:ys) ++ (z:zs)))

uniqList :: (Eq a) => [a] -> [a]
uniqList [] = []
uniqList (x:xs) | elem x xs = [] ++ (uniqList xs)
                | otherwise = [x] ++ (uniqList xs)

sortByValue :: (Ord a) => [a] -> [a]
sortByValue [] = []
sortByValue (x:xs) = sortByValue [el | el <- xs, el < x] ++ [x] ++ sortByValue [el | el <- xs, el > x]