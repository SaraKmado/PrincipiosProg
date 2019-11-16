import System.Environment

main = do
  args <- getArgs
  progName <- getProgName
  print "The arguments are:"
  mapM print args
  print "The program name is:"
  print progName
