import System.IO

main = do
  withFile "oioi.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
