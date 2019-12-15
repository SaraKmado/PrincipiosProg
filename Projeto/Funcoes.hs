module Funcoes
(calculaTempo
,divide
,ganhoAcc
,ganhoAccM
,categoria
,obterPontos
,ganhoAcc' -- usado no tests
,distanciaHorizAux -- usado no tests
,noRaio
,precisao
,removeAll
) where

import Data.List.Split
import Data.Maybe

type Ponto = (Float,Float)

--Self explanatory
calculaTempo :: String -> String -> Int
calculaTempo x y = (d - a) * 60 + (e - b) + (div (fromIntegral (f - c)) (fromIntegral 60))
  where [a,b,c] = tempos x
        [d,e,f] = tempos y

-- Devolve uma lista com formato [HH,MM,SS]
tempos :: String -> [Int]
tempos [] = []
tempos x = map read (splitOn ":" x )

-- Divide cada String da 2a lista em String divididas pelos elementos da 1a string
-- Exemplo: divide [,] ["asdf,qwer","zxcv,asdf"] -> [["asdf",qwer],[zxcv,asdf]]
divide :: String -> [String] -> [[String]]
divide _ [] = [[]]
divide sy (x:xs) = (splitOn sy x):(divide sy xs)

-- Calcula o ganho acumulado
ganhoAcc :: [String] -> Int
ganhoAcc xs = ganhoAcc' (stringsToFloats xs) 0

-- De uma lista de String com formato ["coisa,coisa,Float,etc"] retira todos os Float e devolve-os numa lista
stringsToFloats :: [String] -> [Float]
stringsToFloats [] = []
stringsToFloats (x:xs) = a : stringsToFloats xs
  where splitx = splitOn "," x
        a = read $ splitx !! 2

-- De uma lista de Float, calcula o seu ganho acumulado e arredonda-o a unidade
ganhoAcc' :: [Float] -> Float -> Int
ganhoAcc' [] acc = round acc
ganhoAcc' [x] acc = round acc
ganhoAcc' (x:y:xs) acc = if y > x then ganhoAcc' (y:xs) (acc + y - x) else ganhoAcc' (y:xs) acc

-- Distancia entre 2 pontos
distanciaPontos :: Ponto -> Ponto -> Float
distanciaPontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- Calcula a distancia horizontal total
distanciaHoriz :: [String] -> Float
distanciaHoriz xs = distanciaHorizAux $ listaPontos xs

-- Calcula a distancia horizontal total
distanciaHorizAux :: [Ponto] -> Float
distanciaHorizAux (x:xs) = fst $ foldl (\(acc,y) z -> (acc + distanciaPontos z y,z)) (0.0,x) xs

-- Dada uma lista de String de formato ["Coordenada x, Coordenada y, etc"], retorna todos os seus pontos
listaPontos :: [String] -> [Ponto]
listaPontos xs = foldl (\acc x -> (read ((splitOn' x) !! 0),read ((splitOn' x) !! 1)):acc) [] xs
    where splitOn' = splitOn ","

-- De uma lista de String retorna o ganho acumulado por metro, arredondado a 3 casas decimais
ganhoAccM :: [String] -> Double
ganhoAccM xs = (fromIntegral (round (1000.0 * ((fromIntegral $ ganhoAcc xs) / (85000.0 * distanciaHoriz xs))))) / 1000

-- Determina a categoria de um percurso
categoria :: [String] -> Char
categoria xs
  | ganho < 500 && horiz < 10000 = 'A'
  | ganho < 800 && horiz < 12000 = 'B'
  | ganho < 1500 && horiz < 15000 = 'C'
  | otherwise = 'D'
  where ganho = ganhoAcc xs
        horiz = distanciaHoriz xs

-- Obtem os pontos de interesse de um percurso, com o formato ["ponto1", "ponto2", "etc"]
obterPontos :: [[String]] -> [[String]] -> String
obterPontos gps poi = "[" ++ (format $ obterPontos' (getPontos gps) (getPontos poi) (nomes poi))

-- De uma lista de listas de String com formato [["x1","y1","nome1"],["x2","y2","nome2"],etc] retorna os pontos da lista
getPontos :: [[String]] -> [Ponto]
getPontos [] = []
getPontos (x:xs) = ((read $ x!!0), read $ x!!1) : getPontos xs

-- De uma lista de listas de String com formato [["x1","y1","nome1"],["x2","y2","nome2"],etc] retorna os nomes da lista
nomes :: [[String]] -> [String]
nomes [] = []
nomes (x:xs) = (x!!2) : nomes xs

-- Obtem os pontos de interesse de um percurso, com o formato "ponto1", "ponto2", "etc"]
obterPontos' :: [Ponto] -> [Ponto] -> [String] -> [String]
obterPontos' [] _ _ = []
obterPontos' (g:gps) poi nomes = (obter nomes ps) ++ obterPontos' gps (removeAll poi ps 0) (removeAll nomes ps 0)
    where ps = posicao g poi 0

removeAll :: [a] -> [Int] -> Int -> [a]
removeAll [] _ _ = []
removeAll xs [] _ = xs
removeAll list (p:ps) n = removeAll (remove list (p - n)) ps (n + 1)

remove :: [a] -> Int -> [a]
remove list x = take x list ++ drop (x+1) list

obter :: [a] -> [Int] -> [a]
obter [] _ = []
obter _ [] = []
obter xs (p:ps) = (xs !! p) : obter xs ps

-- Retorna todas as posicoes da lista de pontos a que um ponto esta no raio
posicao :: Ponto -> [Ponto] -> Int -> [Int]
posicao _ [] _ = []
posicao x (y:ys) p = if noRaio x y then (p:posicao x ys (p+1)) else posicao x ys (p+1)

-- Determina se um dado ponto x esta no raio de um ponto y
noRaio :: Ponto -> Ponto -> Bool
noRaio x y = if distanciaPontos x y < precisao y then True else False

-- Determina a precisao de um ponto
precisao :: Ponto -> Float
precisao (x,y) = if a <= b then (0.1 ^ a) else (0.1 ^ b)
    where showx = show x
          showy = show y
          precE string = read $ dropWhile (/= '-') string :: Int
          precP string = length $ last $ splitOn "." string
          a = if elem 'e' showx then precE showx else precP showx
          b = if elem 'e' showy then precE showy else precP showy

-- Transforma uma lista de String numa String de formato "ponto1", "ponto2", "etc"]
format :: [String] -> String
format [] = "]"
format (x:xs) = if length xs == 0 then show x ++ "]" else show x ++ "," ++ format xs
