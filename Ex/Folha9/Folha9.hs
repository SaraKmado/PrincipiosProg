import System.IO
import System.Random
import Data.List

--1
toFile :: Show a => FilePath -> [a] -> IO()
toFile path list = do
  writeFile path (unlines $ [show x| x <- list])

--2
fromFile :: Read a => FilePath -> IO [a]
fromFile path = do
  contents <- readFile path
  let allLines = lines contents
  let result = fromStringToA allLines
  return result

fromStringToA :: Read a => [String] -> [a]
fromStringToA xs = foldr(\x acc -> (read x) : acc) [] xs

--3
sumInts :: FilePath -> IO Int
sumInts path = do
  contents <- readFile path
  let nums = toIntSum $ lines contents
  return nums

toIntSum :: [String] -> Int
toIntSum xs = foldl (\acc x -> (read x) + acc) 0 xs

--4
mergeFiles :: (Read a, Ord a) => FilePath -> FilePath -> IO[a]
mergeFiles path1 path2 = do
  contents1 <- readFile path1
  contents2 <- readFile path2
  let merged = merge (fromStringToA $ lines contents1) (fromStringToA $ lines contents2)
  return merged

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y : merge (x:xs) ys
  | y > x = x : merge xs (y:ys)
  | otherwise = x : y : merge xs ys

--5
---a
filterFiles :: (String -> Bool) -> FilePath -> FilePath -> IO()
filterFiles f source dest = do
  contents <- readFile source
  let result = unlines $ filter f $ lines contents
  writeFile dest result

---b
filterPrefix :: String -> FilePath -> FilePath -> IO()
filterPrefix prefix source dest = filterFiles (isPrefix ('\"' : prefix)) source dest

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] [] = True
isPrefix xs [] = False
isPrefix [] xs = True
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

---c
----ver Filter.hs

--6
----ver wc.hs

--7
----ver menu.hs

--8
rand :: Int -> Int
rand n = fst $ randomR (0,n-1) (mkStdGen 54)
----nao pois recebe sempre o mesmo num para criar o random generator, pelo que cria sempre o mesmo number generator

--9
type Carta = Int
type Mao = [Carta]

---a
mao1 :: Int -> Mao
mao1 n = take n (randomRs (1,52) (mkStdGen 65))

---b
mao2 :: Int -> IO Mao
mao2 n = do
  g <- getStdGen
  return $ take n (randomRs (1,52) (g))

--10
permutar :: Ord a => [a] -> [a]
permutar xs = do
   g <- getStdGen
   let p = snd $ randomR (1,length xs) g
       ys = makeRandomList (length xs) p
   return $ orderBy xs ys

orderBy :: Ord a => [a] -> [Int] -> [a]
orderBy [] [] = []
orderBy xs ys = x : (orderBy (delete x xs) (delete y ys))
 where y = maximum ys
       p = pos ys y 0
       x = xs !! p

pos :: [Int] -> Int -> Int -> Int
pos [] _ _ = error "No"
pos (x:xs) n p = if x == n then p else pos xs n (p+1)

makeRandomList :: Int -> StdGen -> [Int]
makeRandomList 0 _ = []
makeRandomList n g = do
  (fst m) : makeRandomList (n-1) (snd m)
  where m = randomR (1,n^3) g
