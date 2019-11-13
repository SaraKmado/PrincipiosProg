main = do
  putStr "Quantos numeros? "
  n <- read $ getLine
  let nums = readnums n
  print ("A soma e " ++ sum nums)

readnums :: Int -> [Int]
readnums 0 = []
readnums x = (read (getLine)) : readnums (x-1)
