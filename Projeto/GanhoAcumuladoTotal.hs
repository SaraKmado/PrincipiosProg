module GanhoAcumuladoTotal
(ganhoAcc
,ganhoAcc'
) where

import Funcoes

-- Calcula o ganho acumulado
ganhoAcc :: [String] -> Int
ganhoAcc xs = ganhoAcc' (stringsToFloats xs) 0

-- De uma lista de Float, calcula o seu ganho acumulado e arredonda-o a unidade
ganhoAcc' :: [Float] -> Float -> Int
ganhoAcc' [] acc = round acc
ganhoAcc' [x] acc = round acc
ganhoAcc' (x:y:xs) acc = if y > x then ganhoAcc' (y:xs) (acc + y - x) else ganhoAcc' (y:xs) acc
