import System.Environment

main = do
  args <- getArgs
  let pref = args !! 0
  let source = args !! 1
  let dest = args !! 2
  filterPrefix pref source dest

filterFiles :: (String -> Bool) -> FilePath -> FilePath -> IO()
filterFiles f source dest = do
  contents <- readFile source
  let result = unlines $ filter f $ lines contents
  writeFile dest result

filterPrefix :: String -> FilePath -> FilePath -> IO()
filterPrefix prefix source dest = filterFiles (isPrefix ('\"' : prefix)) source dest

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] [] = True
isPrefix xs [] = False
isPrefix [] xs = True
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
