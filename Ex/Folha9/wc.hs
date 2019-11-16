import System.Environment
import System.IO

main = do
  args <- getArgs
  if length args /= 1
    then print "Only one argument"
    else do
      let [name] = args
      readAll 0 0 0 name

readAll :: Int -> Int -> Int -> String -> IO()
readAll l w c name = do
  readLines <- readFile name
  if readLines == ""
    then
        print (show l ++ "          " ++ show w ++ "          " ++ show c)
    else do
        let nLines = length $ lines readLines
            nWords = foldl (\acc x -> acc + (if x == ' ' then 1 else 0)) 0 (readLines)
            nChars = foldl (\acc x -> acc + 1) 0 (readLines)
        readAll (nLines + l) (nWords + w) (nChars + c) name
