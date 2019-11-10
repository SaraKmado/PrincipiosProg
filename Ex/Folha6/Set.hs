module Set(
empty,
Set.null,
singleton,
member,
insert,
fromList,
Set.filter,
remove,
union,
intersection,
difference,
size,
partition
) where

empty :: Ord a => [a]
empty = []

null :: Ord a => [a] -> Bool
null [] = True
null [a] = False

singleton :: Ord a => a -> [a]
singleton a = [a]

member :: Ord a => a -> [a] -> Bool
member y [] = False
member y xs = foldl (\acc x -> if x == y then True else acc) False xs

insert :: Ord a => a -> [a] -> [a]
insert y xs = if member y xs
  then xs
  else (takeWhile (\x -> y >= x) xs) ++ [y] ++ (dropWhile (\x -> y >= x) xs)

fromList :: Ord a => [a] -> [a]
fromList [] = empty
fromList (x:xs) = insert x $ fromList xs

filter :: Ord a => (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

remove :: Ord a => a -> [a] -> [a]
remove x ys = if member x ys
  then (takeWhile (\y -> y < x) ys) ++ (dropWhile (\y -> y <= x) ys)
  else ys

union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union [] xs = xs
union xs [] = xs
union (x:xs) ys = insert x (union xs ys)

intersection :: Ord a => [a] -> [a] -> [a]
intersection _ [] = []
intersection [] _ = []
intersection (x:xs) (y:ys)
  | x == y = x : intersection xs ys
  | x > y = intersection (x:xs) ys
  | otherwise = intersection xs (y:ys)

difference :: Ord a => [a] -> [a] -> [a]
difference [] _ = []
difference xs [] = xs
difference xs ys = [x | x <- xs , not $ member x ys]

size :: Ord a => [a] -> Int
size xs = length xs

partition :: Ord a => (a -> Bool) -> [a] -> ([a],[a])
partition _ [] = ([],[])
partition f xs = (ys, difference xs ys)
  where ys = Set.filter f xs
