module GanhoAcumuladoPorMetro
(ganhoAccM
,distanciaHoriz
,distanciaHorizAux
) where

import Funcoes
import GanhoAcumuladoTotal
import Data.List.Split

-- Calcula a distancia horizontal total
distanciaHoriz :: [String] -> Float
distanciaHoriz xs = distanciaHorizAux $ listaPontos xs

-- Calcula a distancia horizontal total
distanciaHorizAux :: [Ponto] -> Float
distanciaHorizAux (x:xs) = fst $ foldl (\(acc,y) z -> (acc + distanciaPontos z y,z)) (0.0,x) xs

-- Dada uma lista de String de formato ["Coordenada x, Coordenada y", "etc"], retorna todos os seus pontos
listaPontos :: [String] -> [Ponto]
listaPontos xs = foldl (\acc x -> (read ((splitOn' x) !! 0),read ((splitOn' x) !! 1)):acc) [] xs
    where splitOn' = splitOn ","

-- De uma lista de String retorna o ganho acumulado por metro, arredondado a 3 casas decimais
ganhoAccM :: [String] -> Double
ganhoAccM xs = (fromIntegral (round (1000.0 * ((fromIntegral $ ganhoAcc xs) / (85000.0 * distanciaHoriz xs))))) / 1000
