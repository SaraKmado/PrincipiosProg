--1
---a duplica um numero
---bdevolve true se um num for negativo
---c devolve o inverso de um numero
---d divide um num em 2
---e soma 1 a um num
---f adiciona um paragrafo a uma String

--2
----a funçao a e igual a b mas a a recebe um tuplo e a b recebe 2 nums
----a funçao de c retorna uma funçao soma

--3
---a [2,3,4]
---b [True,False,False,False]
---c ["As","artes","dos","alunos"]
---d ["so","saluno","sbem-comportado"]
---e [[1,4],[9,16,25]]
---f [6]
---g [2,4,6,8,10]
---h [9,4,1,1,4,9]
---i [1,4,9]

--4
zipWith1 :: (a->b->c)-> [a] -> [b] -> [c]
zipWith1 f xs [] = []
zipWith1 f [] ys = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

zipWith2 :: (a->b->c)-> [a] -> [b] -> [c]
zipWith2 f xs ys = [f (xs !! x) (ys !! y) | x <- [0..length xs - 1],y <- [0..length ys - 1], x == y]

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys= zipWith1 f xs ys
  where f a b = (a,b)

--5
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f []= []
takeWhile' f (x:xs) = if f x then (x:takeWhile' f xs) else []

--6
dropWhile' :: (a -> Bool)-> [a] -> [a]
dropWhile' f []= []
dropWhile' f  (x:xs) = if f x then dropWhile f xs else x:xs

--7
dropUntil' :: (a -> Bool)-> [a] -> [a]
dropUntil' f []= []
dropUntil' f (x:xs) = if f x then x:xs else dropUntil' f xs

--8
total1 :: (Int -> Int)-> Int -> Int
total1 f x = totalAux1 f x 0

totalAux1 :: (Int -> Int)-> Int -> Int -> Int
totalAux1 f x n = if x == n then f x else f n + totalAux1 f x (n+1)

total2 :: (Int -> Int)-> Int -> Int
total2 f x = sum [f y | y <- [0..x]]

total3 :: (Int -> Int)-> Int -> Int
total3 f x = sum (map f [0..x])

--9
aplica1 :: [a -> a] -> [a] -> [a]
aplica1 [] xs = xs
aplica1 (f:fs) xs = aplica1 fs (map f xs)

aplica2 :: [a -> a] -> [a] -> [a]
aplica2 fs xs = foldr (\f acc -> map f acc) xs (reverse fs)

aplica3 :: [a -> a] -> [a] -> [a]
aplica3 fs xs = foldl (\acc f -> map f acc) xs fs

--10
---a Num a => a -> a
---b 7
---c (Ord a, Num a) => a -> Bool
---d Num a => a -> a -> a
---e Error
---f 10
---g Num a => a -> a -> a
---h (a -> a) -> a -> a
---i devolve Num

--11
----(\a b c -> a*b*c)1 2

--12


--13
isNonBlank :: Char -> Bool
isNonBlank = \c -> elem c [' ','\n','\t']

--14
inv :: (a -> b -> c)-> b -> a -> c
inv f x y = f y x

--15
curry' :: ((a, b)-> c)-> a -> b ->c
curry' f x y = g x y
  where g x y = f (x,y)

uncurry' :: (a -> b -> c)-> (a, b)-> c
uncurry' f (x,y) = g (x,y)
  where g (x,y) = f x y

--16
---a 30
---b
---c
---d
---e

--17
sum' :: Num a => [a] -> a
sum' xs = foldl (+) 0 xs

length' :: [a] -> Int
length' xs = foldr (\x acc -> acc+1) 0 xs

--18
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x:acc else acc) [] xs

--19
binary2decimal :: [Int] -> Int
binary2decimal xs =
  fst (foldl (\(a1,a2) x -> (a1 + x * 2 ^ a2, a2 - 1)) (0,length xs - 1) xs)

--22
selectApply :: (a -> b) -> (a -> Bool) -> [a] -> [b]
selectApply f g xs = [f x | x <- xs, g x]
