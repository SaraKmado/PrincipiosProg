module Main
(main
) where
--run: runhaskell Main BairrosAntigosLisboa.csv LisboaPOI.csv Lisboa.json
import System.Environment
import Data.List.Split
import Funcoes

main = do
  [gps,pontosInteresse,output] <- getArgs
  gpsContents <- readFile gps
  poiContents <- readFile pontosInteresse
  let gpsLines  = lines gpsContents
      gpsLista = init(divide "," gpsLines) -- o init apaga a linha vazia no final do ficheiro
      tempoPassado = calculaTempo (last (head gpsLista)) (last (last gpsLista))
      ganhoAcumulado = ganhoAcc (gpsLines)
      ganhoAcumuladoM = ganhoAccM (gpsLines)
      categor = categoria $ gpsLines
      poiLista = init(divide "," $ lines poiContents)
      allPontos = obterPontos gpsLista poiLista --TODO: esta desordenado
  writeFile output (show "Categoria" ++ ": " ++ "\"" ++ [categor] ++ "\"" ++ ",\n")
  appendFile output (show "Tempo total (m)" ++ ": " ++ show tempoPassado ++ ",\n")
  appendFile output (show "Ganho acumulado" ++ ": " ++ show ganhoAcumulado ++ ",\n")
  appendFile output (show "Ganho acumulado por m" ++ ": " ++ show ganhoAcumuladoM ++ ",\n")
  appendFile output (show "Pontos de Interesse" ++ ": " ++ allPontos)
