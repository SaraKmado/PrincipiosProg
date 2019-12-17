module PontosDeInteresse
(obterPontos
) where

import Funcoes
import Data.List.Split

-- Retorna a lista dos nomes dos pontos de interesse que contem um ponto da rota no seu raio
obterPontos :: [[String]] -> [[String]] -> [String]
obterPontos pontos poi = criaLista (getPontos pontos) (getPontos poi) (nomes poi)

-- Cria a lista dos nomes dos pontos de interesse que contem um ponto da rota no seu raio
criaLista :: [PontoS] -> [PontoS] -> [String] -> [String]
criaLista _ [] [] = []
criaLista (pontos) (p:poi) (n:nomes) = if algumRaio p pontos
  then n : criaLista pontos poi nomes
  else criaLista pontos poi nomes

-- Verifica se um dados ponto de interesse contem um ponto da rota no seu raio
algumRaio :: PontoS -> [PontoS] -> Bool
algumRaio p lista = foldl (\acc ponto -> if noRaio ponto p then True else acc) False lista

-- Verifica se um ponto esta no raio de um ponto de interesse
noRaio :: PontoS -> PontoS -> Bool
noRaio x y = (distanciaPontos (stringToPonto x) (stringToPonto y)) < 0.1 ^ (precisao y)

-- Da que tem menos casas decimais
precisao :: PontoS -> Int
precisao (x,y) = if (a < b) then a else b
  where xise = elem 'e' x
        yise = elem 'e' y
        a = if xise then (0 - read (last (splitOn "e" x))) else length (last (splitOn "." x))
        b = if yise then (0 - read (last (splitOn "e" y))) else length (last (splitOn "." y))

-- De uma lista de listas de String com formato [["x1","y1","nome1"],["x2","y2","nome2"],etc] retorna os pontos da lista
getPontos :: [[String]] -> [PontoS]
getPontos [] = []
getPontos (x:xs) = (x!!0, x!!1) : getPontos xs

-- De uma lista de listas de String com formato [["x1","y1","nome1"],["x2","y2","nome2"],etc] retorna os nomes da lista
nomes :: [[String]] -> [String]
nomes [] = []
nomes (x:xs) = (x!!2) : nomes xs
