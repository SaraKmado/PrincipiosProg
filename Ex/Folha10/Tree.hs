module Tree
(Tree,
emptytree,
treesize,
depth,
flatten,
perfect,
invert,
makeTree,
isIn,
allIn
) where

import Test.QuickCheck

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

instance (Eq a) => Eq (Tree a) where
  t1 == t2 = (allIn t1 t2) && (allIn t2 t1)

instance (Show a) => Show (Tree a) where
  show t = show' t 0

show' :: (Show a)  => Tree a -> Int -> String
show' EmptyTree n = (replicate n '|') ++ "Empty" ++ "\n"
show' (Node l v r) n = (replicate n '|') ++ show v ++ "\n" ++ (show' l (n+1)) ++ (show' r (n+1))

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

--6
---a
-- instance (Arbitrary a) => Arbitrary (Tree a) where
--   arbitrary = do
--     n <- choose (1,2) :: Gen Int
--     case n of
--       1 -> return EmptyTree
--       2 -> do
--         l <- arbitrary
--         r <- arbitrary
--         v <- arbitrary
--         return $ Node l v r

---b

---c
-- instance (Arbitrary a) => Arbitrary (Tree a) where
--   arbitrary = do
--     s <- arbitrary
--     return $ makeTree s

---d
----sized :: (Int -> Gen a) -> Gen a
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    s <- sized (oi)
    return $ makeTree s

oi :: Arbitrary a => Int -> Gen [a]
oi x = do
  n <- choose (0,x) :: Gen Int
  vectorOf x arbitrary

---e
propFlat :: Tree Int -> Bool
propFlat tree = (treesize tree) == (length $ flatten tree)

quickFlat = quickCheck propFlat

propInv :: Tree Int -> Bool
propInv tree = (treesize tree) == (treesize $ invert tree)

quickInv = quickCheck propInv

propMake :: [Int] -> Bool
propMake list = sameElems list (flatten $ makeTree list)

sameElems :: [Int] -> [Int] -> Bool
sameElems xs ys = and $ (map (elem' xs) (filter (elem' ys) xs))
  where elem' zs z = elem z zs

quickMake = quickCheck propMake

allTree = do
  print "propFlat"
  quickFlat
  print "propInv"
  quickInv
  print "propMake"
  quickMake
