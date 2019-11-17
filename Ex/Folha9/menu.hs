--a
linhasComNumeros :: [String] -> [String]
linhasComNumeros strings = zipWith (\x y -> (show y) ++ " - " ++ x) strings [1..]

--b
-- main = do
--   string <- readFile "menu.txt"
--   sequence (map print $ linhasComNumeros $ lines string)

--c
main = do
  string <- readFile "menu.txt"
  mapM print $ linhasComNumeros $ lines string
