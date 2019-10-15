triangulacao :: ((Float, Float), (Float, Float), (Float, Float)) -> (Float, Float)
triangulacao ((x1, y1), (x2, y2), (x3, y3)) = ((x1 + x2 + x3) / 3 , (y1 + y2 + y3) / 3)

distanciaOrigem :: [(Float, Float)] -> [Float]
distanciaOrigem xs = [sqrt (fst x ^ 2 + snd x ^ 2) | x <- xs]

proximoPonto :: [(Float, Float)] -> (Float,Float)
proximoPonto xs =
  (2 * fst (last xs) - fst (last (init xs)), 2 * snd (last xs) - snd (last (init xs)))
