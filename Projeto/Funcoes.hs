module Funcoes
(Ponto
,PontoS
,distanciaPontos
,stringsToFloats
,stringToPonto
,divide
) where

import Data.List.Split

type Ponto = (Float,Float)
type PontoS = (String,String)

-- Distancia entre 2 pontos
distanciaPontos :: Ponto -> Ponto -> Float
distanciaPontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- De uma lista de String com formato ["coisa,coisa,Float,etc"] retira todos os Float e devolve-os numa lista
stringsToFloats :: [String] -> [Float]
stringsToFloats [] = []
stringsToFloats (x:xs) = a : stringsToFloats xs
  where splitx = splitOn "," x
        a = read $ splitx !! 2

-- Converte um tuplo de String num tuplo de Float
stringToPonto :: PontoS -> Ponto
stringToPonto (x,y) = (read x, read y)

-- Divide cada String da 2a lista em String divididas pelos elementos da 1a string
-- Exemplo: divide [,] ["asdf,qwer","zxcv,asdf"] -> [["asdf",qwer],[zxcv,asdf]]
divide :: String -> [String] -> [[String]]
divide _ [] = [[]]
divide sy (x:xs) = (splitOn sy x):(divide sy xs)
