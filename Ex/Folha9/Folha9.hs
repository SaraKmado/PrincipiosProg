import System.IO

--1
toFile :: Show a => FilePath -> [a] -> IO()
toFile path list = do
  writeFile path (toText list)

toText :: Show a => [a] -> String
toText [] = ""
toText (x:xs) = show x ++ "\n" ++ toText xs

--2
fromFile :: Read a => FilePath -> IO [a]
fromFile path = do
  withFile path ReadMode (\handle -> do
    auxFile handle)

auxFile :: Read a => Handle -> [a]
auxFile handle = do
  contents <- read $ hGetContents handle
  return contents
