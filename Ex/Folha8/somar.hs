import System.IO
--recursiva:
--main = do
--  putStr "Quantos numeros? "
--  hFlush stdout
--  ns <- getLine
--  let n = read ns :: Int
--  getNums n []

--getNums :: Int -> [Int] -> IO()
--getNums 0 a = print $ sum a
--getNums n a = do
--  line <- getLine
--  let num = read line :: Int
--  getNums (n-1) (num:a)

--sequence:
main = do
  putStr "Quantos numeros? "
  hFlush stdout
  ns <- getLine
  let n = read ns :: Int
  s <- sequence (createIO n)
  print $ sum $ result s

createIO :: Int -> [IO String]
createIO 0 = []
createIO n = getLine : createIO (n-1)

result :: [String] -> [Int]
result [] = []
result (x:xs) = (read x :: Int) : (result xs)
