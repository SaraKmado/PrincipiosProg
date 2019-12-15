module Funcoes(
calculaTempo
,divide
,ganhoAcc
,ganhoAccM
,categoria
) where

import Data.List.Split

type Ponto = (Float,Float)

calculaTempo :: String -> String -> Int
calculaTempo x y = (d - a) * 60 + (e - b) + (div (fromIntegral (f - c)) (fromIntegral 60))
  where [a,b,c] = tempos x
        [d,e,f] = tempos y

tempos :: String -> [Int]
tempos [] = []
tempos x = map read (splitOn ":" x )

divide :: String -> [String] -> [[String]]
divide _ [] = [[]]
divide sy (x:xs) = (splitOn sy x):(divide sy xs)

ganhoAcc :: [String] -> Float -> Int
ganhoAcc [] acc = round acc
ganhoAcc [x] acc = round acc
ganhoAcc (x:y:xs) acc = if b > a then (ganhoAcc (y:xs) (acc + b - a)) else (ganhoAcc (y:xs) acc)
                       where splitx = splitOn "," x
                             splity = splitOn "," y
                             a = read $ splitx !! 2
                             b = read $ splity !! 2

distanciaPontos :: Ponto -> Ponto -> Float
distanciaPontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

distanciaHoriz :: [String] -> Float
distanciaHoriz xs = distanciaHorizAux $ listaPontos xs

distanciaHorizAux :: [Ponto] -> Float
distanciaHorizAux (x:xs) = fst $ foldl (\(acc,y) z -> (acc + distanciaPontos z y,z)) (0.0,x) xs

listaPontos :: [String] -> [Ponto]
listaPontos xs = foldl (\acc x -> (read ((splitOn "," x) !! 0),read ((splitOn "," x) !! 1)):acc) [] xs

ganhoAccM :: [String] -> Double
ganhoAccM xs = (fromIntegral (round (1000.0 * ((fromIntegral $ ganhoAcc xs 0) / (85000.0 * distanciaHoriz xs))))) / 1000

categoria :: [String] -> Char
categoria xs
  | ganho < 500 && horiz < 10000 = 'A'
  | ganho < 800 && horiz < 12000 = 'B'
  | ganho < 1500 && horiz < 15000 = 'C'
  | otherwise = 'D'
  where ganho = ganhoAcc xs 0
        horiz = distanciaHoriz xs
