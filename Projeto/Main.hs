module Main (
main
) where

import System.Environment
import Data.List.Split
import Funcoes

main = do
  [gps,pontosInteresse,output] <- getArgs
  gpsContents <- readFile gps
  let gpsLines  = lines gpsContents
  let gpsLista = init(divide "," $ gpsLines)
  let tempoPassado = calculaTempo (last (head gpsLista)) (last (last gpsLista))
  let ganhoAcumulado = ganhoAcc (gpsLines) 0
  let ganhoAcumuladoM = ganhoAccM (gpsLines)
  putStrLn (show ganhoAcumuladoM ++ " metros/metros")
  let categor = categoria $ gpsLines
  writeFile output (show ("\"Categoria\": " ++ [categor] ++ "\n"++"\""++"Tempo total (m)\": "
    ++ (show tempoPassado) ++ "\n" ++ "\"Ganho acumulado\": " ++ (show ganhoAcumulado) ++ "\n"
    ++ "\"Ganho acumulado por m: \"" ++ (show ganhoAcumuladoM) ++ "\n"))
