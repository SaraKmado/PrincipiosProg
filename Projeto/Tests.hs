module Tests
(testGanhoAc
,testDistHoriz
,testAdPonto
) where

import GanhoAcumuladoTotal
import GanhoAcumuladoPorMetro
import Funcoes
import Test.QuickCheck
import Data.List

testGanhoAc = quickCheck propGanhoAc

propGanhoAc :: [Float] -> Property
propGanhoAc xs = not (null xs) ==> (ganhoAcc' xs 0) >= 0

testDistHoriz = quickCheck propDistHoriz

propDistHoriz :: [Ponto] -> Property
propDistHoriz xs = not (null xs) ==> distanciaHorizAux xs >= 0

testAdPonto = quickCheck propAdPonto

propAdPonto :: String -> [String] -> Property
propAdPonto x xs = not (null xs) ==> length xs + 1 == length (x:xs)
