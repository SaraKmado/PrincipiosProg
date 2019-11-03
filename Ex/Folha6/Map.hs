module Map --keys not repeated
(empty
, singleton
, insert
, Map.null
, size
, member
, Map.lookup
, delete
, unionWith
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
insert k a (M ms) = M $ (takeWhile (\m -> k > fst m) ms) ++ [(k,a)] ++ (dropWhile (\m -> k >= fst m) ms)

null :: Map k a -> Bool
null (M m) = Prelude.null m

size :: Map k a -> Int
size (M m) = length m

member :: Ord k => k -> Map k a -> Bool
member x (M m) = foldl (\acc (k,a) -> if k == x then True else acc) False m

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k (M m) = Prelude.lookup k m

delete :: Ord k => k -> Map k a -> Map k a
delete k (M ms) = M $ (takeWhile (\m -> k > fst m) ms) ++ (dropWhile (\m -> k >= fst m) ms)

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith _ (M []) (M []) = M []
unionWith _ (M ms) (M []) = M ms
unionWith _ (M []) (M ms) = M ms
unionWith f (M ((m1,m2):ms)) (M ((n1,n2):ns)) = if m1 == n1
  then M $ (m1,f m2 n2) : toList (unionWith f (M ms) (M ns))
  else if m1 > n1
    then M $ (n1,n2) : toList (unionWith f (M ((m1,m2):ms)) (M ns))
    else M $ (n1,n2) : toList (unionWith f (M ms) (M ((n1,n2):ns)))

fromList :: Ord k => [(k,a)] -> Map k a
fromList xs = M xs

toList :: Map k a -> [(k,a)]
toList (M m) = m
