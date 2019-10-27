module Set(
empty,
null,
singleton,
member,
insert,
--fromList, nao percebi
filter,
remove,
union,
intersection,
difference,
size,
partition
) where

empty :: [a]
empty = []

null :: [a] -> Bool
null [] = True
null [a] = Falsr

singleton :: a -> [a]
singleton a = [a]

member :: a -> [a] -> Bool
member y xs = foldl (\acc x -> if x == y then True else acc) False xs

insert :: a -> [a] -> [a]
insert y xs = if y > xs
  then (takeWhile (\x -> y > x) xs) ++ [y] ++ (dropWhile (\x -> y > x) xs)
  else (takeWhile (\x -> y <= x) xs) ++ [y] ++ (dropWhile (\x -> y <= x) xs)

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

remove :: a -> [a] -> [a]
remove x ys = (takeWhile (\y -> y == x) ys) ++ (dropWhile((\y -> y == x) ys))
