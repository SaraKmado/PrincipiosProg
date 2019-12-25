--1
---a
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

---b
replicate' :: Int -> a -> [a]
replicate' n a = if n > 0 then a : (replicate' (n-1) a) else []

---c
maximo :: Ord a => [a] -> a
maximo [] = error "Nao ha maximo na lista vazia"
maximo [x] = x
maximo (x:xs) = if z < x then x else z
  where z = maximo xs

---d
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

---e
substitui :: Eq a => a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui a b (x:xs) = c : substitui a b xs
  where c = if x == a then b else x

---f
altera :: Ord a => [a] -> a -> a -> [a]
altera [] a b = []
altera (x:xs) a b = c : altera xs a b
  where c = if x < a then b else x

---g
multiplos :: [Int] -> Int -> [Int]
multiplos [] x = []
multiplos (x:xs) y = if (mod x y) == 0 then x : multiplos xs y else multiplos xs y

---h
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

---i
potencias :: Integer -> [Integer] -> [Integer]
potencias _ [] = []
potencias b (x:xs) = b^x : potencias b xs

---j
posicoes :: [Int] -> Int -> [Int]
posicoes xs y = posicoesAux xs y 0

posicoesAux :: [Int] -> Int -> Int -> [Int]
posicoesAux [] _ _ = []
posicoesAux (x:xs) y n = if mod x y == 0 then n : posicoesAux xs y (n+1) else posicoesAux xs y (n+1)

---k
frase :: Int -> [(Int,String)] -> String
frase _ [] = []
frase n ((x1,x2):xs) = if x1 == n then x2 ++ frase n xs else frase n xs

---l
trocaPares :: [a] -> [a]
trocaPares [] = []
trocaPares (a:[]) = [a]
trocaPares (a:b:xs) = b:a:trocaPares xs

---m
fusao :: (Ord a, Num b) => [(a,b)] -> [(a,b)] -> [(a,b)]
fusao [] [] = []
fusao [] xs = xs
fusao xs [] = xs
fusao (x:xs) (y:ys) = if fst x > fst y
  then y : fusao (x:xs) ys
  else if fst x < fst y
    then x : fusao xs (y:ys)
    else (fst x,snd x + snd y): fusao xs ys

--2
repBinaria :: Int -> String
repBinaria x = if x < 2 then show x ++ [] else (repBinaria (div x 2) ++ show(mod x 2))

--3
odioso :: Int -> Bool
odioso x = mod (odiosoAux x 0) 2 == 1

odiosoAux :: Int -> Int -> Int
odiosoAux x cont = if x < 2
  then if mod x 2 == 0
    then 0
    else 1
  else if mod x 2 == 0
    then odiosoAux (div x 2) cont
    else odiosoAux (div x 2) cont + 1

--4
basex :: Integer -> Integer -> String
basex n x = if n < x then (change n) else (basex (div n x) x) ++ (change (mod n x))

change :: Integer -> String
change n
  |n == 10 = "A"
  |n == 11 = "B"
  |n == 12 = "C"
  |n == 13 = "D"
  |n == 14 = "E"
  |n == 15 = "F"
  |n == 16 = "G"
  |n == 17 = "H"
  |n == 18 = "I"
  |n == 19 = "J"
  |n == 20 = "K"
  |n == 21 = "L"
  |n == 22 = "M"
  |n == 23 = "N"
  |n == 24 = "O"
  |n == 25 = "P"
  |n == 26 = "Q"
  |n == 27 = "R"
  |n == 28 = "S"
  |n == 29 = "T"
  |n == 30 = "U"
  |n == 31 = "V"
  |n == 32 = "W"
  |n == 33 = "X"
  |n == 34 = "Y"
  |n == 35 = "Z"
  |otherwise = show n ++ []

--5
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y then (x:y:ys) else y: insert x ys

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

--6
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) [y] = if x >= y then y:x:xs else x:merge xs [y]
merge [y] (x:xs) = if x >= y then y:x:xs else x:merge xs [y]
merge (x:xs) (y:ys)
  |x > y = y : merge (x:xs) ys
  |x < y = x : merge xs (y:ys)
  |otherwise = x:y:merge xs ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (fst z)) (mergeSort (snd z))
  where z = halve xs

halve :: [a] -> ([a],[a])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)

--7
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        biggerSorted = quickSort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

randomList :: Int -> [Int]
randomList n = take n randomInfiniteList

randomInfiniteList :: [Int]
randomInfiniteList = iterate f 1234
  where f x = (1343*x + 997) `mod` 1001
