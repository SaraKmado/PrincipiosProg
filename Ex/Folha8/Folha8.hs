import Control.Monad
import System.IO
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
  binSearch 0 maxi 1

binSearch :: Int -> Int -> Int -> IO()
binSearch low up count = do
  let n = div (low + up) 2
  putStrLn ((show n) ++ "? ")
  line <- getLine
  let char = first line
  case char of
    '<' -> do
      binSearch low n (count +1)
    '>' -> do
      binSearch n up (count + 1)
    '=' -> print ("Sucesso apos " ++ (show count) ++ " tentativas")
    _ -> print ("Fode-te")

first :: String -> Char
first (x:xs) = x

--6
----Ver forca.hs
