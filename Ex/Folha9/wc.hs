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
  let myLines2 = myLines
  print ((show $ length $ lines myLines2) ++ "  " ++ (show $ countWords myLines2) ++ "  " ++ (show $ (countChars myLines2)))

countChars :: String -> Int
countChars string = foldl (\acc x -> if (x == '\n' || x == '\"') then acc else acc + 1) 0 string

countWords :: String -> Int
countWords string = length $ words string
