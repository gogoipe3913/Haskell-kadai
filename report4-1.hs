takeXiyjzk :: Int -> Int -> Int -> Int -> [Int]
takeXiyjzk k x y z = chooseNum 0 k x y z

chooseNum :: Int -> Int -> Int -> Int -> Int -> [Int]
chooseNum num k x y z | k > 0 && judgeElem num x y z = num : chooseNum (num + 1) (k - 1) x y z
		                 | k > 0 && (judgeElem num x y z == False) = chooseNum (num + 1) k x y z
		                 | otherwise = []

judgeElem :: Int -> Int -> Int -> Int -> Bool
judgeElem el x y z 
		| el == 1 = True
		| el == 0 = False
		| (mod el x) == 0 = judgeElem (div el x) x y z
		| (mod el y) == 0 = judgeElem (div el y) x y z
		| (mod el z) == 0 = judgeElem (div el z) x y z
    | otherwise = False