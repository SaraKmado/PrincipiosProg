--a
distancia :: [(Float,Float)] -> Float
distancia [] = 0
distancia [x] = 0
distancia [x,y] = calcDist x y
distancia (x:y:xs) = calcDist x y + distancia (y:xs)

calcDist :: (Float,Float) -> (Float,Float ) -> Float
calcDist (x1,y1) (x2,y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

--b
fundePercursos :: [(Float,Float)] -> [(Float,Float)] -> (Float,Float) -> [(Float,Float)]
fundePercursos [] [] x = x:[]
fundePercursos [] xs x = x:xs
fundePercursos xs [] x = x:xs
fundePercursos (x:xs) (y:ys) z
  |distancia [x,z] > distancia [y,z] = z:fundePercursos (x:xs) ys y
  |otherwise = z: fundePercursos xs (y:ys) x

--c
adicionaParagem :: [(Float,Float)] -> (Float,Float) -> [(Float,Float)]
adicionaParagem (x:y:xs) z =
   adicionaAux (x:y:xs) z (findPlace (y:xs) z 2 (distancia [x,z,y]) 1)

adicionaAux :: [(Float,Float)] -> (Float,Float) -> Int -> [(Float,Float)]
adicionaAux [x] y _ = [y,x]
adicionaAux (x:xs) y n = if n == 0
  then y:x:xs
  else x:adicionaAux xs y (n-1)

--Function that finds the position that a new point belongs
findPlace :: [(Float,Float)] -> (Float,Float) -> Int -> Float -> Int -> Int
findPlace [] _ _ _ minPos = minPos
findPlace [x] _ _ _ minPos = minPos
findPlace (x:y:xs) z currPos minDistance minPos =
  if d < minDistance
    then findPlace (y:xs) z (currPos + 1) d currPos
    else findPlace (y:xs) z (currPos + 1) minDistance minPos
    where d = distancia [x,z,y]
