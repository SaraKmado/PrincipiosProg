import Control.Monad
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
palindrome :: String -> Bool
palindrome xs = xs == reverse xs
---a
isP1 = do
  line <- getLine
  print (if palindrome line then "Sim" else "Nao")
---b
isP2 = do
  line <- getLine
  when (not $ null line) $ do
    print (if palindrome line then "Sim" else "Nao")
    isP2
---c
isP3 = interact palindromeWrite

palindromeWrite allLines = unlines $ foldr (\x acc -> if palindrome x then "Sim" : acc else "Nao" : acc) [] (lines allLines)

--3
---a
printEven :: Int -> IO()
printEven x = if even x then print "Par" else print "Impar"

---b
showParity1 :: [Int] -> IO()
showParity1 [] = error "Empty list"
showParity1 [x] = printEven x
showParity1 (x:xs) = do
  printEven x
  showParity1 xs

---c
showParity2 :: [Int] -> IO()
showParity2 xs = mapM_ printEven xs

--4
----ler um numero n. depois ler n inteiros e soma los
----ver somar.hs

--5
guess :: Int -> IO()
guess maxi = do
  let med = div maxi 2
  let num = binSearch med 0
  print ("Sucesso apos " ++ (show num) ++ " tentativas")

--binSearch :: Int -> Int -> Int
binSearch n count = do
  print ((show n) ++ "? ")
  char <- getChar
  if char == '>'
    then return $ binSearch (3 * n / 2) (n+1)
    else if char == '<'
      then return $  binSearch (n/2) (n+1)
      else if char == '='
        then return n
        else error "Char nao suportado"
