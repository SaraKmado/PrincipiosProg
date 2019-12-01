import System.Environment
import Data.List.Split
--read from console: file csv percurso, file csv pontos interesse, file json
--first two read, last write

main = do
  [percurso, pInteresse,resultado] <- getArgs
  --read first file
  --format: latitude,longitude,elevacao,hora\n etc
  percContents <- readFile percurso
  let percLines = lines percContents
  let divided = divide percLines
  --format:  [[latitude,longitude,elevacao,hora],[latitude,longitude,elevacao,hora],etc], all String
  print divided
  print ""
  --read second file
  --format: latitude,longitude,nome\n etc
  intContents <- readFile pInteresse
  let intLines = lines intContents
      dividedInt = divide intLines
  --format: [[latitude,longitude,nome],[latitude,longitude,nome]etc] all Strings
  print dividedInt
  --ver tempo total: diferenca entre ultimo e 1o registo no 1o ficheiro

  --ver ganho acumulado total: metros totais subidos. Descidas nao contam. arredondamento simetrico a unidade
  --ex: subir 2m, descer 100m, subir 3m -> esforço de 5m

   --ver ganho acumulado por metro: metros subidos / metros totais andados na horizontal. como as coordenadas sao em graus,
   --cada grau e aprox 85km. arredondamento simetrico a 3a casa decimal

   --ver categora: de A a D
   -- <500m ganho && <10km distancia -> A
   -- <800m ganho && <12km distancia -> B
   -- <1500m ganho && <15km distancia -> C
   -- otherwise D

   --ver pontos de interesse: lista dos pontos de interesse do percurso em que esse percurso passa num raio de vizinhança
   --raio de vizinhança: definido pela resolucao das coordenadas do ponto

divide :: [String] -> [[String]]
divide [] = [[]]
divide (x:xs) = splitOn "," x : divide xs
