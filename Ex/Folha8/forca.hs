import System.IO
import Data.Maybe

maxAttempts = 6 --variavel globar. nice
--isto acabou por ter mais que o que o exercicio pedia. oh well
--permite adivinhar letras ou a palavra toda

main = do
  print "Primeiro Jogador, pense numa palavra:"
  hSetEcho stdin False
  word <- getLine
  hSetEcho stdin True
  case (length word) of
      0 -> print "Bela piada. Se nao queres jogar entao nao jogues fds"
      1 -> print "Queres mesmo ser fodido! Proxima vez tenta uma palavra maior"
      2 -> print "Queres mesmo ser fodido! Proxima vez tenta uma palavra maior"
      otherwise ->  do
        let currWord = replicate (length word) '-'
        print currWord
        print "Segundo Jogador, tente adivinhar:"
        jogo 1 word currWord

jogo :: Int -> String -> String -> IO()
jogo n word curr = if (n-1) == maxAttempts
  then print $ "Maximo de tentativas atingido. A palavra era " ++ word
  else
    do
      print ("Tentativa " ++ (show n))
      cs <- getLine
      if length cs > 1
        then
          if word == cs
            then print "Acertou!"
            else do
              print "Errou..."
              print $ "A palavra era " ++ word

      else do
        let c = first cs
        let result = fromMaybe "" (jogar word curr c)
        if result == ""
          then do
            print word
            print "Acertou!"
            else do
              print result
              if elem c word
                then jogo (n) word result


                else jogo (n+1) word result

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
first [] = error "Mais que uma letra seu estupido"
first [x] = x
first (x:xs) = error "So uma letra seu estupido"
