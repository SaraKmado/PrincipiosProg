import System.Random
import Control.Monad (when)

main = do
  gen <- getStdGen
  let (randNumber,_) = randomR (1,10) gen :: (Int,StdGen)
  print "Which number from 1 to 10 am I thinking of?"
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then print "Correct!"
      else print $ "Wrong. It was " ++ show randNumber
    newStdGen
    main
