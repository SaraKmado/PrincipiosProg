import Test.QuickCheck

--grupo 1
--a
primeiroUltimo :: [a] -> a
primeiroUltimo (x:xs) = if even (length (x:xs))
  then x
  else ultimo xs

--b
ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

--c
minmax :: Ord a => [a] -> (a,a)
minmax xs = (minimo xs,maximo xs)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = if x > mini then mini else x
  where mini = minimo xs

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = if x < maxi then maxi else x
  where maxi = maximo xs

--d
minmax' :: Ord a => [a] -> (a,a)
minmax' (x:xs) = foldl (\(mini,maxi) y -> (if y < mini then y else mini, if y > maxi then y else maxi)) (x,x) xs

--grupo 2
--Um Metro (comboio) é caracterizado pelo número de carruagens
data Metro = Metro Int deriving Show
--Uma linha de metro é representada por uma String
type Linha = String
--A Alocação descreve que metros estão atribuídos a que linhas e que metros estão livres
data Alocacao = Alocacao [(Metro,Linha)] [Metro] deriving Show

umaAloc = Alocacao [(Metro 6, "Amarela")] [Metro 3, Metro 5]

--a
existeMetroLivre :: Int -> Alocacao -> Bool
existeMetroLivre x (Alocacao _ livres) = foldl (\acc (Metro y) -> if y > x then True else acc) False livres

--b
alocaMetro :: Int -> Linha -> Alocacao -> Alocacao
alocaMetro n linha (Alocacao os ls) = Alocacao ((Metro size,linha) : os) (remover size ls)
  where size = findSize n ls

findSize :: Int -> [Metro] -> Int
findSize s ((Metro x) : xs) = if s < x then x else findSize s xs

remover :: Int -> [Metro] -> [Metro]
remover s ((Metro x) : xs) = if s == x then xs else (Metro x):remover s xs

--c
instance Eq Alocacao where
  (Alocacao xs ys) == (Alocacao as bs) = (contaLivre ys + contaBusy xs) == (contaLivre bs + contaBusy as)

contaLivre :: [Metro] -> Int
contaLivre xs = foldl (\acc (Metro x) -> acc + x) 0 xs

contaBusy :: [(Metro,String)] -> Int
contaBusy xs = foldl (\acc (Metro x,string) -> acc + x) 0 xs

--grupo 3

instance Arbitrary Alocacao where
  arbitrary = do
    numLivre <- arbitrary
    numOcupado <- arbitrary
    let livres = makeLivres numLivre
    let ocupados = makeOcupados numOcupado
    return $ Alocacao ocupados livres

makeLivres :: Int -> [Metro]
makeLivres 0 = []
makeLivres n = do
  x <- arbitrary
  return $ x : makeLivres (n-1)

makeOcupados :: Int -> [(Metro,String)]
makeOcupados 0 = []
makeOcupados n = do
  x <- arbitrary
  s <- arbitrary
  return $ (x, s) : makeOcupados (n-1)

instance Arbitrary Metro where
  arbitrary = do
     x <- arbitrary
     return $ Metro x

-- prop_livre_vazio :: Alocacao -> Property
-- prop_livre_vazio (Alocacao xs ys) = existeMetroLivre (Alocacao xs ys) ==> not $ null ys
--
-- prop_alocar_numero :: Alocacao -> Property
-- prop_alocar_numero al x =>

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
