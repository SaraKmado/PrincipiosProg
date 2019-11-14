import System.IO
import Data.Maybe
main = do
  print "Primeiro Jogador, pense numa palavra:"
  hSetEcho stdin False
  word <- getLine
  hSetEcho stdin True
  let currWord = replicate (length word) '-'
  print currWord
  print "Segundo Jogador, tente adivinhar:"
  jogo 1 word currWord

jogo :: Int -> String -> String -> IO()
jogo n word curr = if (n-1) == 6
  then print $ "Maximo de tentativas atingido. A palavra era " ++ word
  else
    do
      print ("Tentativa " ++ (show n))
      cs <- getLine
      let c = first cs
      let result = fromMaybe "" (jogar word curr c)
      if result == ""
        then do
          print word
          print "Acertou!"
          else do
            print result
            jogo (n+1) word result


jogar :: String -> String -> Char -> Maybe String
jogar word curr char = if result == word
  then Nothing
  else Just result
  where result = complete word curr char

complete :: String -> String -> Char -> String
complete [] [] _ = []
complete (x:xs) (y:ys) c = if c == x
  then x:complete xs ys c
  else y:complete xs ys c

first :: String -> Char
first [] = error "No"
first [x] = x
first (x:xs) = error "No"
