main = do
  putStr "Quantos numeros? "
  ns <- getLine
  let n = read ns
  let nums = readnums n
  print ("A soma e " ++ (show $ sum nums))

readnums :: Int -> [Int]
readnums 0 = []
readnums x = do
  ns <- getLine
  let n = read ns
  n : readnums (x-1)
