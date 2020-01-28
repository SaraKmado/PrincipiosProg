--grupo 4
intercalar :: a -> [a] -> [a]
intercalar _ [] = [] --i1
intercalar _ [y] = [y] --i2
intercalar x (y:ys) = y : x : intercalar x ys --i3

--mostrar:
--length (intercalar x ys)
  -- | length ys > 0 = 2 * length ys - 1
  -- | otherwise = 0

--funcao length:
length' :: [a] -> Int
length' [] = 0 --l1
length' (x:xs) = 1 + length' xs --l2

--Caso []: mostrar que  length (intercalar x []) = 0

--length (intercalar x [])
--length [] -- i1
--0 -- l1
----------------

--Caso [x]: mostrar que length (intercalar x [y]) = 2 * (length [y]) - 1

--length (intercalar x [y])
--length [y] --i2
--1 + length [] --l2
--1 + 0 --l1
--1

--length [y]
--1 + length [] --l2
--1 + 0 --l1
--1

--1 = 2 * 1 - 1 = 1, como queriamos demonstrar
-------------------

--Caso (x:xs): mostrar que length (intercalar x (y:ys)) + 2 = 2 * (length (y:ys)) + 1

--length (intercalar x (y:ys))
--length (x:y:intercalar x ys) -- i3
--1 + length (y:intercalar x ys) -- l2
--2 + length (intercalar x ys) --l2
--2 * length (y:ys) + 1 -- HI
