--Sara Queimado 52806

--para usar: fold, map, filter
--usado: fold, map, filter

--A
distancia :: [(Float,Float)] -> Float
distancia [] = 0
distancia (x:xs) = fst (foldl (\(acc,y) p -> (acc + dist y p,p)) (0,x) xs)

dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

--B
minimaDistanciaA :: (Float,Float) -> [(Float,Float)] -> Float 
minimaDistanciaA _ [] = error "a lista e vazia"
minimaDistanciaA x xs = minimum (map (dist x) xs)

--C
evitaPontos :: Float -> [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)]
evitaPontos d [] xs = xs

evitaPontos d _ [] = []

evitaPontos d [x] ys = filter (\y -> (dist x y) > d) ys
evitaPontos d (x:xs) ys = (exists (evitaPontos d xs ys) (filter (\y -> (dist x y) > d) ys))

exists :: [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)]
exists xs ys = [x | x <- xs, elem x ys]
