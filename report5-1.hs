mss :: [Int] -> Int
mss (x:xs) = maximum (map sum (setBefore (x:xs)))

setBefore :: [Int] -> [[Int]]
setBefore xs = [] : setEach xs

setEach :: [Int] -> [[Int]]
setEach = foldr (++) [] . scanr (\a b -> [a] : map (a:) b) []