main = do
  maxiS <- getLine
  let maxi = read maxiS :: Int
  let med = div maxi 2
  let num = binSearch med 0
  print ("Sucesso apos " ++ (show num) ++ " tentativas")

binSearch :: Int -> Int -> Int
binSearch n
