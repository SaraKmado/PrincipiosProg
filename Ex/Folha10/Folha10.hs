import Test.QuickCheck
import Data.List as List
import Folha4 as F
import Data.Maybe
import Set
import Tree

--1
---a
testPar :: Int -> Bool
testPar x = if even x then odd (x+1) else even (x+1)

quickPar :: IO()
quickPar = quickCheck testPar

---b
mx :: Int -> Int
mx x = (2^x) - 1

isPrime :: Int -> Bool
isPrime x = foldl (\acc y -> if mod x y == 0 then False else acc) True [2..(x-1)]

testM :: Int -> Property
testM x = x >= 0 ==> (if isPrime x then isPrime (mx x) else True)

quickM = quickCheck testM

---c
----If even, halve, if odd, 3n+1. Get to 1
collatz :: Int -> [Int]
collatz x = collatz' x []

collatz' :: Int -> [Int] -> [Int]
collatz' 1 xs = xs ++ [1]
collatz' x xs = if even x
  then collatz' (div x 2) (xs ++ [x])
  else collatz' (3*x+1) (xs ++ [x])

collatzTest :: Int -> Property
collatzTest x = x > 0 ==> elem 1 $ take 100000 $ collatz x

quickCollatz = quickCheck collatzTest

collatzTest2 x = x > 0 ==>
  classify (x == 1) "One" $
  elem 1 $ take 100000 $ collatz x

quickCollatz2 = quickCheck collatzTest2

collatzTest3 x = x > 0 ==>
  classify (x == 1) "One" $
  collect (length $ collatz x) $
  elem 1 $ take 100000 $ collatz x

quickCollatz3 = quickCheck collatzTest3

---d
mdc :: Int -> Int -> Int
mdc x y = if (x > y)
  then if (mod x y == 0) then y else mdc y (mod x y)
  else if (mod y x == 0) then x else mdc x (mod y x)

mmc :: Int -> Int -> Int
mmc x y = firstCommon x y

firstCommon :: Int -> Int -> Int
firstCommon x y = firstCommon' (takeWhile (<= m) $ getMul x m) (takeWhile (<= m) $ getMul y m)
  where m = x * y

firstCommon' :: [Int] -> [Int] -> Int
firstCommon' xs ys = head $ dropWhile (isNotElem xs) ys

isNotElem :: [Int] -> Int -> Bool
isNotElem xs x = not $ elem x xs

getMul :: Int -> Int -> [Int]
getMul x n = foldr (\y acc -> (y * x) : acc) [] [1..n]

testMult :: Int -> Int -> Property
testMult x y = x > 0 && y > 0 ==> x * y == (mdc x y) * (mmc x y)

quickMult = quickCheck testMult

--2
---a
testRevLen :: [a] -> Bool
testRevLen xs = length xs == (length $ reverse xs)

---b
testRevInv :: (Eq a, Ord a) => [a] -> Bool
testRevInv xs = areEqual xs $ reverse $ reverse xs

testRevInv2 :: [Int] -> Bool
testRevInv2 xs = areEqual xs $ reverse $ reverse xs

areEqual :: (Eq a) => [a] -> [a] -> Bool
areEqual [] [] = True
areEqual [] _ = False
areEqual _ [] = False
areEqual (x:xs) (y:ys) = (x == y) && (areEqual xs ys)
---c
testRevPerm :: Eq a => [a] -> Bool
testRevPerm xs = elem (reverse xs) (permutations xs)

----Corre forever
-- testRevPerm2 :: [Int] -> Bool
-- testRevPerm2 xs = elem (reverse xs) (permutations xs)

---d
testRevI :: Ord a => [a] -> Int -> Property
testRevI xs n = (n >= 0) && (length xs > 0) && (n < length xs) ==> (xs !! n) == ((reverse xs) !! (l - n - 1))
  where l = length xs

testRevI2 ::[Int] -> Int -> Property
testRevI2 xs n = (n >= 0) && (length xs > 0) && (n < length xs) ==> (xs !! n) == ((reverse xs) !! (l - n - 1))
  where l = length xs

--3
testSum :: (Num a, Eq a) => [a] -> Bool
testSum xs = (sum xs) == (F.sum' xs)

---a
testReplicate :: Eq a => Int -> a -> Property
testReplicate n a = n >= 0 ==> (replicate n a) == (F.replicate' n a)

---b
testMaximo :: Ord a => [a] -> Property
testMaximo xs = (length xs > 0) ==> (maximum xs) == (maximo xs)

---c
testElem :: Eq a => a -> [a] -> Bool
testElem x xs = (elem x xs) == (F.elem' x xs)

---d
testSubstitui :: Eq a => a -> a -> [a] -> Bool
testSubstitui n m xs = (testSubLength n m xs) && (testSubElem n m xs)

testSubLength :: Eq a => a -> a -> [a] -> Bool
testSubLength n m xs = (length xs) == (length $ substitui n m xs)

testSubElem :: Eq a => a -> a -> [a] -> Bool
testSubElem n m xs = if elem n xs
  then elem m ys
  else (elem m xs == elem m ys)
  where ys = substitui n m xs

---e
testAltera :: Ord a => [a] -> a -> a -> Bool
testAltera xs n m = (testAltLength xs n m)

testAltLength :: Ord a => [a] -> a -> a -> Bool
testAltLength xs n m = (length xs) == (length $ altera xs n m)

testAltera2 :: [Int] -> Int -> Int -> Property
testAltera2 xs n m = n >= 0 ==> (testAltLength xs n m) && (testAltSub xs xs n m n)

testAltSub :: [Int] -> [Int] -> Int -> Int -> Int -> Bool
testAltSub xs ys 0 m o = ys == (altera xs o m)
testAltSub xs ys n m o = testAltSub xs (substitui n m ys) (n-1) m o

---g
multiplosTest :: [Int] -> Int -> Property
multiplosTest xs x = x > 0 ==> (testElemM xs x) && (testMul xs x)

testElemM :: [Int] -> Int -> Bool
testElemM xs x = allElem xs (multiplos xs x)

allElem :: [Int] -> [Int] -> Bool
allElem xs [] = True
allElem [] xs = False
allElem xs (y:ys) = (elem y xs) && (allElem xs ys)

testMul :: [Int] -> Int -> Bool
testMul xs x = allMul (multiplos xs x) x

allMul :: [Int] -> Int -> Bool
allMul [] x = True
allMul (x:xs) y = if mod x y == 0 then allMul xs y else False

---h
zipTest :: (Eq a, Eq b) => [a] -> [b] -> Bool
zipTest xs ys = (F.zip' xs ys) == (zip xs ys)

zipTest2 :: [Int] -> [Char] -> Bool
zipTest2 xs ys = (F.zip' xs ys) == (zip xs ys)

--i
potenciasTest :: Integer -> [Integer] -> Property
potenciasTest x xs = allPos xs
  ==> (testLengthP x xs) && (testValuesP x xs)

allPos :: (Num a, Ord a) => [a] -> Bool
allPos xs = foldl (\acc x -> if x > 0 then acc else False) True xs

testLengthP :: Integer -> [Integer] -> Bool
testLengthP x xs = (length xs) == (length $ potencias x xs)

testValuesP :: Integer -> [Integer] -> Bool
testValuesP x xs = testValPAux x xs (potencias x xs)

testValPAux :: Integer -> [Integer] -> [Integer] -> Bool
testValPAux _ [] [] = True
testValPAux z (x:xs) (y:ys) = if z^x == y then testValPAux z xs ys else False

---j
posicoesTest :: [Int] -> Int -> Property
posicoesTest xs x = x > 0 ==> testElemPos x xs (F.posicoes xs x)

testElemPos :: Int -> [Int] -> [Int] -> Bool
testElemPos _ [] [] = True
testElemPos _ xs [] = True
testElemPos x xs (y:ys) = if (mod (xs !! y) x == 0) then (testElemPos x xs ys) else False

--j
fraseTest :: Int -> [(Int,String)] -> Property
fraseTest x xs = noneEmpty word ==>
  F.frase x xs == (unwords' $ snd $ unzip $ unJustify $ zipWith (\a b -> if a == x then Just (x,b) else Nothing) nums word)
  where nums = fst $ unzip xs
        word = snd $ unzip xs

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ unwords' xs

noneEmpty :: [String] -> Bool
noneEmpty [] = True
noneEmpty (x:xs) = if x == "" then False else noneEmpty xs

unJustify :: [Maybe (Int, String)] -> [(Int, String)]
unJustify [] = []
unJustify (Nothing : xs) = unJustify xs
unJustify ((Just (x,y)) : xs) = (x,y) : unJustify xs

---l
trocaParesTest :: Eq a => [a] -> Bool
trocaParesTest xs = (testPermutations xs (trocaPares xs)) && (testSmall xs) && ((length xs) == (length $ trocaPares xs))

testPermutations :: Eq a => [a] -> [a] -> Bool
testPermutations xs ys = elem xs (permutations ys)

testSmall :: Eq a => [a] -> Bool
testSmall xs = testPermutationsAll (divideIn2s xs) $ divideIn2s ys
  where ys = trocaPares xs

divideIn2s :: [a] -> [[a]]
divideIn2s [] = []
divideIn2s [a] = [[a]]
divideIn2s (a:b:xs) = [a,b] : divideIn2s xs

testPermutationsAll :: Eq a => [[a]] -> [[a]]-> Bool
testPermutationsAll [] [] = True
testPermutationsAll (a:xs) (b:ys) = if testPermutations a b
  then testPermutationsAll xs ys
  else False

---m
fusaoTest :: (Num b) => [(Char,b)] -> [(Char,b)] -> Bool
fusaoTest xs ys = (testOrder ordsO ordsT)
  where zs = fusao xs ys
        ordsO = ((fst $ unzip xs) ++ (fst $ unzip ys))
        numsO = ((snd $ unzip xs) ++ (snd $ unzip ys))
        ordsT = fst $ unzip zs
        numsT = snd $ unzip zs

testOrder :: Ord a => [a] -> [a] -> Bool
testOrder xs ys = (List.sort xs) == ys

--4
data Point = Point (Float,Float) deriving Show-- (x,y)
data Shape = Circle Point Float | Rectangle Point Point | Triangle Point Point Point deriving Show
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

---a
instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Point (x,y)

instance Arbitrary Shape where
  arbitrary = do
    n <- choose (1,3) :: Gen Int
    case n of
      1 -> do
        x <- arbitrary
        y <- arbitrary
        return $ Circle x y
      2 -> do
        x <- arbitrary
        y <- arbitrary
        return $ Rectangle x y
      3 -> do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Triangle x y z

---b
prop_non_neg_perim :: Shape -> Property
prop_non_neg_perim x = positiveRadius x ==> perimeter x >= 0

positiveRadius :: Shape -> Bool
positiveRadius (Circle _ x) = x > 0
positiveRadius _ = True

quickNonNeg = quickCheck prop_non_neg_perim

---c


--5
---a
data Set a = S [a]

instance (Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
    s <- arbitrary
    return $ S s

---b
---- empty, construtora
---- Set.null, observadora
---- singleton, derivada
---- member, observadora
---- insert, construtora
---- fromList, construtora
---- Set.filter, construtora
---- remove, construtora
---- union, derivada
---- intersection, derivada
---- difference, construtora
---- size, observadora
---- partition, construtora

---c
prop_null_empty :: Bool
prop_null_empty = Set.null empty

prop_null_insert :: Int -> [Int] -> Property
prop_null_insert x xs = ordered xs ==> not $ Set.null $ Set.insert x xs

prop_null_fromList :: String -> Bool
prop_null_fromList xs = List.null xs == (Set.null $ fromList xs)

instance Show (a -> b) where
  show x = "Func"

prop_null_filter :: (Int -> Bool) -> [Int] -> Bool
prop_null_filter f xs = if Set.null xs then Set.null $ Set.filter f xs else True

prop_null_remove :: Int -> [Int] -> Property
prop_null_remove x xs = ordered xs ==> if Set.null xs then Set.null $ remove x xs else True

prop_null_difference :: [Int] -> [Int] -> Property
prop_null_difference xs ys = (ordered xs && ordered ys) ==> if Set.null xs
  then Set.null $ difference xs ys
  else if Set.null ys
    then xs == (difference xs ys)
    else True

prop_member_empty :: Int -> Bool
prop_member_empty x = not $ member x empty

prop_member_insert :: Int -> [Int] -> Property
prop_member_insert x xs = (ordered xs) ==> member x (Set.insert x xs)

prop_member_fromList :: Int -> [Int] -> Bool
prop_member_fromList x xs = elem x xs == member x (fromList xs)

prop_member_filter :: Int -> [Int] -> (Int -> Bool) -> Bool
prop_member_filter x xs f = (f x && member x xs) == (member x $ Set.filter f xs)

prop_member_remove :: Int -> [Int] -> Property
prop_member_remove x xs = (ordered xs) ==> not $ member x $ remove x xs

prop_member_difference :: Int -> [Int] -> [Int] -> Property
prop_member_difference x xs ys = (ordered ys && ordered xs) ==> if (member x xs && member x ys)
  then not $ member x $ difference xs ys
  else if member x xs
    then member x $ difference xs ys
    else not $ member x $ difference xs ys

prop_size_empty :: Bool
prop_size_empty = (Set.size empty) == 0

prop_size_insert :: Int -> [Int] -> Property
prop_size_insert x xs = (ordered xs) ==> if member x xs then (size xs) == (size $ Set.insert x xs) else (size xs + 1) == (size $ Set.insert x xs)

prop_size_fromList :: [Int] -> Bool
prop_size_fromList xs = (length xs) >= (size $ fromList xs)

prop_size_filter :: Int -> [Int] -> (Int -> Bool) -> Bool
prop_size_filter x xs f = size xs >= (size $ Set.filter f xs)

prop_size_remove :: Int -> [Int] -> Property
prop_size_remove x xs = (ordered xs) ==> if member x xs then size xs - 1 == (size $ remove x xs) else size xs == (size $ remove x xs)

prop_size_difference :: [Int] -> [Int] -> Property
prop_size_difference xs ys = (ordered xs && ordered ys) ==>
  (size $ difference xs ys) <= (size xs)

---d
prop_singleton :: Int -> Bool
prop_singleton x = singleton x == ((Set.insert x empty)) && (Set.null $ remove x $ singleton x)

prop_intersection :: [Int] -> [Int] -> Property
prop_intersection xs ys = (ordered xs && ordered ys) ==>
  (intersection xs ys) == (Set.filter (member' (Set.filter (member' xs) ys)) xs)
    where member' ls l = member l ls

prop_difference :: [Int] -> [Int] -> Property
prop_difference xs ys = (ordered xs && ordered ys) ==>
  (difference xs ys) == (Set.filter (notMember $ intersection xs ys) xs)
  where notMember ls l = not $ member l ls

prop_union :: [Int] -> [Int] -> Property
prop_union xs ys = (ordered xs && ordered ys) ==>
  (Set.union xs ys) == (fromList (xs ++ ys))

---e
----Criamos um set de 2 elementos
prop_empty_insert :: Int -> Bool
prop_empty_insert x = singleton x == Set.insert x empty

prop_insert_remove :: Int -> [Int] -> Property
prop_insert_remove x xs = ordered xs ==>
  if member x xs
    then (remove x $ Set.insert x xs) == (remove x xs)
    else (remove x $ Set.insert x xs) == xs


ordered xs = and $ zipWith (<) xs (tail xs)

testAll = do
  print "prop_null_empty"
  quickCheck prop_null_empty
  print "prop_null_insert"
  quickCheck prop_null_insert
  print "prop_null_fromList"
  quickCheck prop_null_fromList
  print "prop_null_filter"
  quickCheck prop_null_filter
  print "prop_null_remove"
  quickCheck prop_null_remove
  print "prop_null_difference"
  quickCheck prop_null_difference
  print "prop_member_empty"
  quickCheck prop_member_empty
  print "prop_member_insert"
  quickCheck prop_member_insert
  print "prop_member_fromList"
  quickCheck prop_member_fromList
  print "prop_member_filter"
  quickCheck prop_member_filter
  print "prop_member_remove"
  quickCheck prop_member_remove
  print "prop_member_difference"
  quickCheck prop_member_difference
  print "prop_size_empty"
  quickCheck prop_size_empty
  print "prop_size_insert"
  quickCheck prop_size_insert
  print "prop_size_fromList"
  quickCheck prop_size_fromList
  print "prop_size_filter"
  quickCheck prop_size_filter
  print "prop_size_remove"
  quickCheck prop_size_remove
  print "prop_size_difference"
  quickCheck prop_size_difference
  print "prop_singleton"
  quickCheck prop_singleton
  print "prop_intersection"
  quickCheck prop_intersection
  print "prop_difference"
  quickCheck prop_difference
  print "prop_union"
  quickCheck prop_union
  print "prop_empty_insert"
  quickCheck prop_empty_insert
  print "prop_insert_remove"
  quickCheck prop_insert_remove

--6
---a ver Tree.hs

---b
