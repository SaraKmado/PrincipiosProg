import System.Environment

main = do
  [path] <- getArgs --read from stdOut separated with paragraph
  things <- readFile path
  print $ goThrough $ map read $ lines things

goThrough :: [Int] -> String
goThrough = findBestPath [] 0 'a'

findBestPath :: String -> Int -> Char -> [Int] -> String
findBestPath result _ _ [0] = result
findBestPath result 0 _ (x:y:xs) = if (x) > (y)
    then findBestPath ("b1"++result) 1 'b' xs
    else findBestPath ("a1"++result) 1 'a' xs
findBestPath result n c (z:x:y:xs) =
  if c == 'a'
    then if x < y + z
      then findBestPath (result++", a"++(show $ n + 1)) (n+1) 'a' xs
      else findBestPath (result++", b"++(show $ n + 1)) (n+1) 'b' xs
    else if y < x + z
      then findBestPath (result++", b"++(show $ n + 1)) (n+1) 'b' xs
      else findBestPath (result++", a"++(show $ n + 1)) (n+1) 'a' xs
