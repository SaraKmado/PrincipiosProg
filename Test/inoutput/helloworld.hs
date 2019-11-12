import Data.Char

main = do
  putStrLn "hey, what's your name?"
  name <- getLine
  let screamN = map toUpper name
  putStrLn (screamN ++ "!")
  putStrLn ("Hi " ++ (nick name) ++ ", sup?")

nick name = "bro " ++ name
