module Tests
(testGanhoAc
,testDistHoriz
--,testAdPonto
,testRaio
,testPrecisao
) where

import Funcoes
import Test.QuickCheck

testGanhoAc :: [Float] -> Property
testGanhoAc xs = not (null xs) ==> (ganhoAcc' xs 0) >= 0

testDistHoriz :: [(Float,Float)] -> Property
testDistHoriz xs = not (null xs) ==> distanciaHorizAux xs >= 0

-- Um ponto esta no raio de si mesmo
testRaio :: (Float,Float) -> Bool
testRaio x = noRaio x x

-- Testar a precisao
testPrecisao :: (Float,Float) -> Bool
testPrecisao x = precisao x > 0

-- Testar o removeAll. TODO: remover?
testRemoveAll :: [Int] -> [Int] -> Property
testRemoveAll xs ys = ordered ys && not (null xs) && not (null ys) && checkSize ys (length xs) && posIndex ys && allDistinct xs [] ==> allElem xs ys (removeAll xs ys 0)

ordered :: [Int] -> Bool
ordered [] = True
ordered (x:xs) = fst $ foldl (\(acc,prev) curr -> (if curr < prev then False else acc,curr)) (True,x) xs

allDistinct :: [Int] -> [Int] -> Bool
allDistinct [] _ = True
allDistinct (x:xs) ys = if elem x ys then False else allDistinct xs (x:ys)

checkSize :: [Int] -> Int -> Bool
checkSize [x] y = x < y
checkSize (x:xs) y = x < y && checkSize xs y

posIndex :: [Int] -> Bool
posIndex [x] = x >= 0
posIndex (x:xs) = x >= 0 && posIndex xs

allElem :: [Int] -> [Int] -> [Int] -> Bool
allElem [] _ _ = False
allElem _ [] _ = True
allElem xs (y:ys) zs = (not $ elem (xs !! y) zs) && (allElem xs ys zs)
