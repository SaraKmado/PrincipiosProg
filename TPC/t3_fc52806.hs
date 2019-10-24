--Sara Queimado 52806

--A
distancia :: [(Float,Float)] -> Float
distancia (x:xs) = fst (foldl (\(acc,y) p -> (acc + dist y p,p)) (0,x) xs)

dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
