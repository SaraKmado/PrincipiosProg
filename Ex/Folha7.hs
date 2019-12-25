--1
{-# Language InstanceSigs #-}
data Point = Point (Float,Float) -- (x,y)
data Shape = Circle Point Float | Rectangle Point Point | Triangle Point Point Point
-- Cirlce as Center(x,y) radius, Rectangle as (x1,y1) (x2,y2), Triangle as its 3 points

perimeter :: Shape -> Float
perimeter (Circle _ radius) = 2 * radius * pi
perimeter (Rectangle (Point (x1,y1)) (Point (x2,y2))) = 2 * abs(x2-x1) + 2 * abs(y2-y1)
perimeter (Triangle (Point (x1,y1)) (Point (x2,y2)) (Point (x3,y3))) = dist (x1,y1) (x2,y2) + dist (x2,y2) (x3,y3) + dist (x1,y1) (x3,y3)

dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

isRegular :: Shape -> Bool
isRegular (Circle _ _) = True
isRegular (Rectangle (Point (x1,y1)) (Point (x2,y2))) = x1 - x2 == y1 - y2
isRegular (Triangle (Point p1) (Point p2) (Point p3)) =
  dist2 (Point p1) (Point p2) == dist2 (Point p2) (Point p3) && dist2 (Point p1) (Point p3) == dist2 (Point p1) (Point p2)

dist2 :: Point -> Point -> Float
dist2 (Point (x1,y1)) (Point (x2,y2)) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

--2
data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add x Zero = x
add Zero y = y
add (Succ x) y = add x (Succ y)

minus :: Nat -> Nat -> Nat
minus x Zero = x
minus Zero y = Zero
minus (Succ x) (Succ y) = minus x y

natPred :: Nat -> Nat
natPred Zero = error "Undefined"
natPred (Succ x) = x

sub :: Nat -> Nat -> Nat
sub x Zero = x
sub Zero x = error "Undefined"
sub x y = sub (natPred x) (natPred y)

mult :: Nat -> Nat -> Nat
mult x Zero = Zero
mult Zero x = Zero
mult x (Succ Zero) = x
mult (Succ Zero) x = x
mult x y = add x (mult x (natPred y))

pot :: Nat -> Nat -> Nat
pot x Zero = Succ Zero
pot Zero x = Zero
pot x (Succ Zero) = x
pot (Succ Zero) x = Succ Zero
pot x y = mult x (pot x (natPred y))

fact :: Nat -> Nat
fact Zero = Succ Zero
fact (Succ Zero) = Succ Zero
fact x = mult x (fact (natPred x))

remnat :: Nat -> Nat -> Nat
remnat Zero x = Zero
remnat x Zero = error "Undefined"
remnat x (Succ Zero) = x
remnat x y = if minus x y == Zero
  then x
  else remnat (minus x y) y

quotnat :: Nat -> Nat -> Nat
quotnat x Zero = error "Undefined"
quotnat Zero x = Zero
quotnat x y = if lessThan x y
  then Zero
  else Succ (quotnat (sub x y) y)

lessThan :: Nat -> Nat -> Bool
lessThan Zero Zero = False
lessThan x Zero = False
lessThan Zero (Succ x) = True
lessThan x y = lessThan (natPred x) (natPred y)

--3
infixr 5 :-:
data Set a = Empty | a :-: (Set a)

empty :: Ord a => Set a
empty = Empty

setnull :: Ord a => Set a -> Bool
setnull Empty = True
setnull (x :-: xs) = False

singleton :: Ord a => a -> Set a
singleton x = x :-: Empty

member :: Ord a => a -> Set a -> Bool
member x Empty = False
member x (y :-: xs) = if x == y
  then True
  else member x xs

insert :: Ord a => a -> Set a -> Set a
insert x Empty = singleton x
insert x (y :-: ys) = if x == y
  then y :-: ys
  else if x < y
    then x :-: (y :-: ys)
    else y :-: (insert x ys)


fromList :: Ord a => [a] -> Set a
fromList [] = Empty
fromList (x:xs) = insert x $ fromList xs

setfilter :: Ord a => (a -> Bool) -> Set a -> Set a
setfilter _ Empty = Empty
setfilter f (x :-: xs) = if f x
  then x :-: setfilter f xs
  else setfilter f xs

remove :: Ord a => a -> Set a -> Set a
remove x Empty = Empty
remove x (y :-: ys) = if not $ member x (y:-:ys)
  then y:-:ys
  else if x == y
    then ys
    else y :-: (remove x ys)

union :: Ord a => Set a -> Set a -> Set a
union Empty Empty = Empty
union Empty xs = xs
union xs Empty = xs
union (x:-:xs) ys = insert x (union xs ys)

intersection :: Ord a => Set a -> Set a -> Set a
intersection _ Empty = Empty
intersection Empty _ = Empty
intersection (x :-: xs) (y :-: ys)
  | x == y = x :-: intersection xs ys
  | x > y = intersection (x:-:xs) ys
  | otherwise = intersection xs (y:-:ys)

difference :: Ord a => Set a -> Set a -> Set a
difference Empty _ = Empty
difference xs Empty = xs
difference (x:-:xs) ys = if member x ys
  then difference xs ys
  else x :-: (difference xs ys)

size :: Ord a => Set a -> Int
size Empty = 0
size (x:-:xs) = 1 + size xs

partition :: Ord a => (a -> Bool) -> Set a -> (Set a, Set a)
partition _ Empty = (Empty,Empty)
partition f xs = (ys,difference xs ys)
  where ys = setfilter f xs

--4
infixr 5 :+
data Map k a = Emptym | (k,a) :+ (Map k a)

emptym :: Map k a
emptym = Emptym

singletonm :: k -> a -> Map k a
singletonm k a = (k,a) :+ Emptym

insertm :: Ord k => k -> a -> Map k a -> Map k a
insertm k a (Emptym) = singletonm k a
insertm k a ((x,y) :+ xs) = if k > x
  then (x,y) :+ insertm k a xs
  else if k < x
    then (k,a) :+ ((x,y) :+ xs)
    else (k,a) :+ xs

nullm :: Map k a -> Bool
nullm Emptym = True
nullm _ = False

sizem :: Map k a -> Int
sizem Emptym = 0
sizem (x:+xs) = 1 + sizem xs

memberm :: Ord k => k -> Map k a -> Bool
memberm k Emptym = False
memberm k (x:+xs) = if (k < fst x)
  then False
  else if k == fst x
    then True
    else memberm k xs

lookupm :: Ord k => k -> Map k a -> Maybe a
lookupm k xs = Prelude.lookup k $ toListm xs

deletem :: Ord k => k -> Map k a -> Map k a
deletem k Emptym = emptym
deletem k (x:+xs) = if not (memberm k (x:+xs))
  then x:+xs
  else if k == fst x
    then xs
    else x :+ (deletem k xs)

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith _ Emptym Emptym = Emptym
unionWith _ Emptym xs = xs
unionWith _ xs Emptym = xs
unionWith f ((x,a):+xs) ((y,b):+ys) = if (x == y)
  then (x,f a b) :+ unionWith f xs ys
  else if x > y
    then (y,b) :+ unionWith f ((x,a) :+ xs) ys
    else (x,a) :+ unionWith f xs ((y,b) :+ ys)

fromListm :: Ord k => [(k,a)] -> Map k a
fromListm [] = Emptym
fromListm (x:xs) = x :+ fromListm xs

toListm :: Map k a -> [(k,a)]
toListm Emptym = []
toListm (x:+xs) = x : toListm xs

--5
data Tree a = EmptyTree | Node (Tree a) a (Tree a)

emptytree :: Tree a
emptytree = EmptyTree

treesize :: Tree a -> Int
treesize EmptyTree = 0
treesize (Node l v r) = 1 + treesize l + treesize r

depth :: Tree a -> Int
depth EmptyTree = 0
depth (Node l v r) = 1 + max (depth l) (depth r)

flatten :: Tree a -> [a]
flatten EmptyTree = []
flatten (Node l v r) =[v] ++ (flatten l) ++ (flatten r)

perfect :: Tree a -> Bool
perfect EmptyTree = True
perfect (Node l v r) = depth l == depth r && perfect l && perfect r

invert :: Tree a -> Tree a
invert EmptyTree = EmptyTree
invert (Node l v r) = Node (invert r) v (invert l)

makeTree :: [a] -> Tree a
makeTree [] = emptytree
makeTree (x:xs) = Node (makeTree (take n xs)) x (makeTree (drop n xs))
  where n = div (length xs) 2

isIn :: Eq a => a -> Tree a -> Bool
isIn _ EmptyTree = False
isIn x (Node l v r)
  | x == v = True
  | otherwise = (isIn x l) || (isIn x r)

allIn :: Eq a => Tree a -> Tree a -> Bool
allIn EmptyTree t = True
allIn t EmptyTree = False
allIn t1 t2 = foldl(\acc x -> if elem x ts2 then acc else False) True ts1
  where ts1 = flatten t1
        ts2 = flatten t2

--6
----No.

--7
instance (Eq a) => Eq (Tree a) where
    t1 == t2 = (allIn t1 t2) && (allIn t2 t1)

--8
instance (Show a) => Show (Tree a) where
  show t = show' t 0

show' :: (Show a)  => Tree a -> Int -> String
show' EmptyTree n = (replicate n ' ') ++ "Empty" ++ "\n"
show' (Node l v r) n = (replicate n ' ') ++ show v ++ "\n" ++ (show' l (n+1)) ++ (show' r (n+1))

--9
instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

--10
instance (Eq a) => Eq (Set a) where
  Empty == Empty = True
  Empty == (x:-:xs) = False
  (x:-:xs) == Empty = False
  (x:-:xs) == (y:-:ys) = if (x == y) then (xs == ys) else False

instance (Show a) => Show (Set a) where
  show Empty = "{}"
  show xs = show'' xs False

show'' :: Show a => Set a -> Bool -> String
show'' xs False = "{" ++ show'' xs True
show'' (x:-:Empty) True = (show x) ++ "}"
show'' (x:-:xs) True = (show x) ++ "," ++ show'' xs True

--11
instance Functor Set where
  fmap :: (Ord a, Ord b) =>  (a -> b) -> Set a -> Set b
  fmap _ Empty = Empty
  fmap f (x :-: Empty) = singleton (f x)
  fmap f (x :-: xs) = insert (f x) (fmap f xs)

--12
instance (Eq k, Eq a) => Eq (Map k a) where
  Emptym == Emptym = True
  Emptym == xs = False
  xs == Emptym = False
  xs == ys = length x == length y && and [x !! z == y !! z | z <- [1..length x]]
    where x = toListm xs
          y = toListm ys
--13
instance (Show k, Show a) => Show (Map k a) where
  show Emptym = "{}"
  show (m :+ xm) = (foldl (\acc x -> acc ++", " ++(show $ fst x) ++ ": " ++ (show $ snd x)) ("{" ++ (show $ fst m) ++ ": " ++ (show $ snd m)) xs) ++ "}"
    where xs = toListm xm

--14
class Visible a where
  toString :: a -> String
  dimension :: a -> Int

instance Visible Char where
  toString a = [a]
  dimension a = 1

instance Visible Bool where
  toString True = "True"
  toString False = "False"
  dimension True = 1
  dimension False = 0

instance (Visible a) => Visible [a] where
  toString [] = ""
  toString [x] = toString x
  toString (x:xs) = (toString x) ++ ", " ++ toString xs
  dimension xs = dimension xs * length xs

instance (Visible a, Visible b) => Visible (a,b) where
  toString (a,b) = "(" ++ (toString a) ++ "," ++ (toString b) ++")"
  dimension (a,b) = 2

--15
--instance (Ord a,Ord b) => Ord (a,b) where
--  (a,b) < (c,d) = if a == c then b < d else a < c
--  (a,b) > (c,d) = if a == c then b > d else a > c
--  x <= y = not $ x > y
--  x >= y = not $ x < y
