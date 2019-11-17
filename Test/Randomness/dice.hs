import System.Random
import System.Environment

main = do
  [sides] <- getArgs
  let n = read sides :: Int
  g <- getStdGen
  roll n g

roll :: Int -> StdGen -> IO()
roll n g = do
  print $ fst (randomR (1,n) g)
