module Map --keys not repeated
(empty
, singleton
, insert
--, Map.null
--, size
, member
--, Map.lookup
--, delete
--, unionWith
, fromList
, toList
) where

data Map k a = M [(k,a)]

empty :: Map k a
empty = M []

singleton :: k -> a -> Map k a
singleton k a = M [(k,a)]

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a (M []) = M [(k,a)]
insert k a (M m) = if k == (fst $ head m)
  then M ((k,a) : tail m)
  else if k > (fst $ head m)
    then insert k a (M (tail m))
    else M ((k,a) : m)

member :: Ord k => k -> Map k a -> Bool
member x (M m) = foldl (\acc (k,a) -> if k == x then True else acc) False m

fromList :: Ord k => [(k,a)] -> Map k a
fromList xs = M xs

toList :: Map k a -> [(k,a)]
toList (M m) = m
