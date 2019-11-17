import System.Environment
import System.IO
import Data.List

main = do
  args <- getArgs
  if length args /= 1
    then print "Only one argument"
    else do
      let [name] = args
      readAll name

readAll :: FilePath -> IO()
readAll name = do
  myLines <- readFile name
  let yooo = myLines
  print ((show $ length $ lines yooo) ++ "  " ++ (show $ countWords yooo) ++ "  " ++ (show $ (countChars yooo)))

countChars :: String -> Int
countChars string = foldl (\acc x -> if (x == '\n' || x == '\"') then acc else acc + 1) 0 string

countWords :: String -> Int
countWords string = length $ words string
