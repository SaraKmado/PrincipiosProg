import Control.Monad

main = do
  line <- getLine
  when (not $ null line) $ do
      print $ reverseWords line
--      return 4
      print "--"
      main


reverseWords :: String -> String
reverseWords = unwords.map reverse.words
