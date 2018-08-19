msp :: [Int] -> Int
msp xs = maximum (map product (setBefore xs))

setBefore :: [Int] -> [[Int]]
setBefore xs = [] : setEach xs

setEach :: [Int] -> [[Int]]
setEach = foldr (++) [] . scanr (\a b -> [a] : map (a:) b) []