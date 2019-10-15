--1
soma1 :: Int -> Int -> Int -> Int
soma1 a b c = a+b+c

soma2 :: Int -> Int -> Int -> Int
soma2 a b c = if(a > 0 && b > 0 && c > 0)
  then a + b + c
  else 0

--2
distancia :: Int -> Int -> Int -> Bool
distancia a b c = abs (a - b) < c

--3
addDigit :: Int -> Int -> Int
addDigit a b = if a >= 0
  then a * 10 + b
  else a * 10 - b

--4
--- a 2, b 2, c 1, d 2, e 1, f 0, g 1, h 2

--5
---a
moreThanTen :: [a] -> Bool
moreThanTen list = length list > 10
---b
notNull :: [a] -> Bool
notNull list = not (null list)

---c
takeFirstAndLast :: String -> String
takeFirstAndLast string = init (tail string)

---d
getSecond :: [a] -> a
getSecond list = head (tail list)

---e
getSecondToLast :: [a] -> a
getSecondToLast list = last (init list)

---f
getN :: [a] -> Int -> a

----n between 0 and (length list -1)
getN list n = last (take (n+1) list)

getSecond' :: [a] -> a
getSecond' list = getN list 1


getSecondToLast' :: [a] -> a
getSecondToLast' list = getN list (length list - 2)

---g
invertExceptFirst :: [a] -> [a]
invertExceptFirst list = (head list) : reverse (tail list)

---h
sum5 :: [Double] -> Double
sum5 list = sum (take 5 list)

---i
sumN :: [Double] -> Int -> Double
sumN list n = sum (take n list)

sum5' :: [Double] -> Double
sum5' list = sumN list 5

---j
listStuff :: [Int] -> [Int] -> Bool

listStuff l1 l2 = not (null l1)  &&
  not (null l2)                  &&
  (last l1) == (last l2)         &&
  (length l1) == (length l2)

--6
---a
prefix :: String -> String -> Bool
prefix s1 s2 = length s1 < length s2 &&
  take (length s1) s2 == s1

---b
suffix :: String -> String -> Bool
suffix s1 s2 = prefix (reverse s1) (reverse s2)

---c
prefixOrSuffix :: String -> String -> Bool
prefixOrSuffix s1 s2 = prefix s1 s2 || suffix s1 s2

--7
----mn = min, mx = max, dv = div
particao mn mx dv = [mn + (mx - mn) * (x :: Double) / (dv :: Double) | x <- [0..dv]]

--8
--- a [2,4,6], b [4,16,36,64], c "6789", d [(1,1),(1,2),(1,3),(3,1),(3,2),(3,3)], e [(1,1),(1,2),(1,3),(3,1),(3,2),(3,3)]
---nota: para o c, import Data.Char

--9
ex9 = sum [x ^ 2 | x <- [1..2]]

--10
pitagoricos lim = [(x,y,z) | x <- [1..lim], y <- [1..lim], z <- [1..lim],
  x < y, x^2 + y^2 == z^2]

--11
---a
fatores x = [y | y <- [1..x - 1], x `mod` y == 0]

---b
perfeitos x = [y | y <- [1..x], sum (fatores y) == y]


--12
pot2 = [2^x | x <- [1..]]

--13
produtoEscalar x y = sum [fst a * snd a | a <- zip x y]

--14
reproduz lista = concat [replicate x x| x <- lista]

--15
--exercicio15 :: [(Int, Int)]
exercicio15 = concat [[(x,y) | x <- [1,2,3]] | y <- [4,5,6]]

--16
----a,b,d,e,f,h

--17
pares n = [(i, j) | i <- [1..n], j <- [1..n], i /= j]

--18
fromTo :: Int -> Int -> String -> String
fromTo x y string = reverse (take (y - x + 1) (reverse (take (y + 1) string)))

tail' string = fromTo 1 ((length string) - 1) string
init' string = fromTo 0 ((length string) - 2) string

--19
matID :: Int -> [[Int]]
matID n = [[getNum x y | y <- [1..n]] | x <-[1..n]]

getNum x y = if x == y
  then 1
  else 0

--20
