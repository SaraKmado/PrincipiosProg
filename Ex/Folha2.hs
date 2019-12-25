--1
---a (Int,Int,Int), b Int, c [(Int,Int)], d (String,Int,[(Stirng,Int)]], e [(String,Int,[(String,Int)])],f [String],[[String]]

--2
---a,b,d,e,,g,h

--3
---a False,b GT, c "True or False", d "As armas e os barões", e Exceção, f True, g [False,True], h [], i 5.5, j 5.5, k 5.5

--4
--- Os parametros sao ordenaveis, ou seja, podem ser maiores, menores ou iguais que uns e outros, e o resultado e um numero

--5
---a ([Int],[Int]), ([1,2,3,4],[5])
---b [Char], ['c'..'x']
---c [Char], "pp"

--6
---a
f1 :: (Ord a) => a -> a -> Bool
f1 x y = x < y

---b
f2 :: (Ord a) => a -> a -> Bool -> Bool
f2 x y z = x == y || z

---c
f3 :: Bool -> Bool -> Bool -> Bool
f3 x y z = x == (y || z)

---d
f4 :: (Show a) => a -> [Char] -> [Char]
f4 x y = show x ++ y

---e
f5 :: (Show a) => [a] -> [a] -> [Char]
f5 x y = show (x ++ y)

---f
f6 :: (Ord a, Num a) => a -> a -> a -> Bool
f6 x y z = x + y > z

--7
---a T, b T, c F, d T, e F, f T, g F

--8
--- a a, b (b,a), c (a,b), d Num a => a, e Bool, f a
