import Test.QuickCheck
import Control.Monad

data Semaforo = Verde | Amarelo| Vermelho deriving (Eq,Show)

seguinte :: Semaforo -> Semaforo
seguinte Verde     = Amarelo
seguinte Amarelo   = Vermelho
seguinte Vermelho = Verde

instance Arbitrary Semaforo where
  arbitrary = elements [Verde, Amarelo, Vermelho]

-- instance Arbitrary Semaforo where
--   arbitrary = oneof [return Verde, return Amarelo,return Encarnado]

-- instance Arbitrary Semaforo where
--   arbitrary = do
--     n <- choose (1,3) :: Gen Int
--     return $ case n of
--       1 -> Verde
--       2 -> Amarelo
--       3 -> Vermelho

prop_semaforo_circular :: Semaforo -> Bool
prop_semaforo_circular s = (seguinte . seguinte . seguinte) s == s

quickCirc = quickCheck prop_semaforo_circular

data Doc = Vazio | Frase String | Concat Doc Doc deriving Show

-- instance Arbitrary Doc where
--   arbitrary = do
--     n <- choose (1,3) :: Gen Int
--     case n of
--       1 -> return Vazio
--       2 -> do s <- arbitrary
--               return $ Frase s
--       3 -> do d1 <- arbitrary :: Gen Doc
--               d2 <- arbitrary :: Gen Doc
--               return $ Concat d1 d2

instance Arbitrary Doc where
  arbitrary = oneof [
    return Vazio,
    liftM Frase arbitrary,
    liftM2 Concat arbitrary arbitrary
    ]

dimensao :: Doc -> Int
dimensao Vazio = 0
dimensao (Frase _) = 1
dimensao (Concat d1 d2) = dimensao d1 + dimensao d2

espalmar :: Doc -> [String]
espalmar d = filter (not . null) (paraLista d) where
  paraLista Vazio = []
  paraLista (Frase s) = [s]
  paralista (Concat d1 d2) = paraLista d1 ++ paraLista d2

nos :: Doc -> Int
nos Vazio = 1
nos (Frase _) = 1
nos (Concat d1 d2) = nos d1 + nos d2 + 1

prop_distribuicao d = collect (nos d) True

quickDist = quickCheck prop_distribuicao 

prop_dimensao_espalmar d = (dimensao d) == (length $ espalmar d)

quickDim = quickCheck prop_dimensao_espalmar
