import Data.Char

--1
---a
first :: (a,b) -> a
first(a,b) = a
---b
switch2 :: (a,b) -> (b,a)
switch2 (a,b) = (b,a)
---c
firstOf3 :: (a,b,c) -> a
firstOf3 (a,b,c) = a
---d
switch3 :: (a,b,c) -> (b,a,c)
switch3 (a,b,c) = (b,a,c)
---e
secondOfList :: [a] -> a
secondOfList (_ : a : [_]) = a
---f
secondOf2OfList :: [(a,b)] -> b
secondOf2OfList ((a,b) : xs) = b
----what to do if empty list?

--2
somaVec :: (Double,Double) -> (Double,Double) -> (Double,Double)
somaVec (x,y) (a,b) = (x+a,y+b)

--3
---fazem a mesma coisa mas o b. so funciona com listas de int. o a. nao tem cabecalho

--4
---Sao iguais exceto o c., que nunca chega a 2a linha de codigo e e por isso infinito
---o a. e o c. usam pattern matching, o b uma comparacao, e o d. uma guarda

--5
quadrant :: (Ord a,Num a) => (a,a) -> Int
quadrant (x,y)
    |x > 0 && y > 0 = 1
    |x < 0 && y > 0 = 2
    |x < 0 && y < 0 = 3
    |x > 0 && y < 0 = 4
    |otherwise      = 0 --not in quadrant

--6
ordinalPrefix :: Int -> String
ordinalPrefix a
    |(length (show a) == 1 && last (show a) == '1') = show a ++ "st"
    |(length (show a) == 1 && last (show a) == '2') = show a ++ "nd"
    |(length (show a) == 1 && last (show a) == '3') = show a ++ "rd"

    |(last (show a) == '1' && last (init (show a)) /= '1') = show a ++ "st"
    |(last (show a) == '2' && last (init (show a)) /= '1') = show a ++ "nd"
    |(last (show a) == '3' && last (init (show a)) /= '1') = show a ++ "rd"

    |otherwise = show a ++ "th"

--7
--leetSpeak L3G17 M3G4 F1X3 feito no final do semestre
leetSpeak :: String -> String
leetSpeak [] = []
leetSpeak (x:xs) = change x : leetSpeak xs

change :: Char -> Char
change c
  |c == 'a' = '4'
  |c == 'i' = '1'
  |c == 't' = '7'
  |c == 'o' = '0'
  |c == 's' = '5'
  |c == 'e' = '3'
  |otherwise = toUpper c

-- meu leetSpeak original merdoso
-- leetSpeak :: [Char] -> [Char]
-- leetSpeak a = leetAux a 0
--
-- leetAux :: [Char] -> Int -> [Char]
-- leetAux a i = if i == (length a)
--   then a
--   else leetAux (change a i) (i+1)
--
-- --import Data.Char for toUpper
-- change :: [Char] -> Int -> [Char]
-- change a i
--   |c == 'a' = ((take i a)++'4':[])++(drop (i+1) a)
--   |c == 'i' = ((take i a)++'1':[])++(drop (i+1) a)
--   |c == 't' = ((take i a)++'7':[])++(drop (i+1) a)
--   |c == 'o' = ((take i a)++'0':[])++(drop (i+1) a)
--   |c == 's' = ((take i a)++'5':[])++(drop (i+1) a)
--   |c == 'e' = ((take i a)++'3':[])++(drop (i+1) a)
--   |otherwise = ((take i a)++((toUpper c):[]))++(drop (i+1) a)
--   where c = a !! i

--8
safeTailA :: [a] -> [a]
safeTailB :: [a] -> [a]
safeTailC :: [a] -> [a]

safeTailA a = if null a
  then []
  else tail a

safeTailB a
  |null a = []
  |otherwise = tail a

safeTailC [] = []
safeTailC a = tail a

--9
halveA :: [a] -> ([a],[a])
halveB :: [a] -> ([a],[a])

halveA a = (take i a, drop i a)
  where i = (div (length a) 2)

halveB a = let i = div (length a) 2
           in (take i a, drop i a)

--10
roots a b c = (i,j)
  where i = ((-b) - sqrt(b * b - 4 * a * c))/(2*a)
        j = ((-b) + sqrt(b * b - 4 * a * c))/(2*a)

--11
roots' a b c
  |isNaN i && isNaN j = []
  |i == j = [i]
  |otherwise = [i,j]
  where i = ((-b) - sqrt(b * b - 4 * a * c))/(2*a)
        j = ((-b) + sqrt(b * b - 4 * a * c))/(2*a)

--12
----operadores infixos sao definidos entre parentesis
(\/) True x = True
(\/) x True = True
(\/) x False = x


--13
func :: [(Integer,Char)]
func = zip xs ys
  where xs = tail [0,1,2,3]
        ys = init ['a','b','c','d']
---- [(1,'a'),(2,'b'),(3,'c')] e [a] -> [b] -> [(a,b)]
