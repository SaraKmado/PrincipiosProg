
--1
writePrimes :: [Int] -> IO()
writePrimes xs = do
  let t = generateTo (1 + maximum xs) 2 []
  let text = foldl (\acc x -> acc ++ (show x) ++ "th prime is " ++ (show $ t !! x) ++ "\n") "" xs
  print text

generateTo :: Int -> Int -> [Int]-> [Int]
generateTo maxi curr list = if maxi == length list
  then list
  else if prime curr
    then generateTo maxi (curr + 1) (list ++ [curr])
    else generateTo maxi (curr + 1) list

prime :: Int -> Bool
prime x = foldl (\acc y -> if mod x y == 0 then False else acc) True [2..(x-1)]

--2
