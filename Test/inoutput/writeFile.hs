import System.IO

main = do
  writeFile "oioi.txt" (toText [1,2,3,4,5])

toText :: Show a => [a] -> String
toText [] = ""
toText (x:xs) = show x ++ "\n" ++ toText xs
