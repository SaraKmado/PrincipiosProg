module TempoTotal
(calculaTempo
) where

import Funcoes
import Data.List.Split

--Self explanatory
calculaTempo :: String -> String -> Int
calculaTempo x y = (d - a) * 60 + (e - b) + (div (fromIntegral (f - c)) (fromIntegral 60))
  where [a,b,c] = tempos x
        [d,e,f] = tempos y

-- Devolve uma lista com formato [HH,MM,SS]
tempos :: String -> [Int]
tempos [] = []
tempos x = map read (splitOn ":" x )
