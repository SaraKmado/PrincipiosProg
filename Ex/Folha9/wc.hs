import System.Environment
import System.IO

main = do
  args <- getArgs
  if length args /= 1
    then print "Only one argument"
    else do
      let [name] = args
      handle <- openFile name ReadMode
      readAll 0 0 0 nadle

readAll :: Int -> Int -> Int -> Handle -> IO()
readAll l w c name = do
  myLines <- readFile name
  if length myLines == 0
    then print (show l ++ "  " ++ show w ++ "  " ++ show c)
    else do
      print (show l ++ "  " ++ show w ++ "  " ++ show c)
      let ch = length myLines
      let wo = length $ lines myLines
      let li = 1
      readAll (l + li) (w + wo) (c + ch) name

getNumWords :: String -> Int
getNumWords string = foldl (\acc x -> if x == ' ' then acc + 1 else acc) 0 string
