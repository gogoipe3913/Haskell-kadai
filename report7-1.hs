downto0 :: Int -> [Int]
downto0 n | n >= 0 = n : downto0 (n - 1)
	        | otherwise = []

zipPlus :: [Int] -> [Int] -> [Int]
zipPlus _ _ = []
zipPlus (x:xs) (y:ys) = (x + y) : zipPlus xs ys