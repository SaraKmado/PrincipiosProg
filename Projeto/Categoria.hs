module Categoria
(categoria
) where

import GanhoAcumuladoTotal
import GanhoAcumuladoPorMetro

-- Determina a categoria de um percurso
categoria :: [String] -> Char
categoria xs
  | ganho < 500 && horiz < 10000 = 'A'
  | ganho < 800 && horiz < 12000 = 'B'
  | ganho < 1500 && horiz < 15000 = 'C'
  | otherwise = 'D'
  where ganho = ganhoAcc xs
        horiz = distanciaHoriz xs
