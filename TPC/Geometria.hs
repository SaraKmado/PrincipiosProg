{- |
Module      :  Geometria.hs
Description :  Funções úteis para trabalhar com pontos 2D
Copyright   :  (c) Departamento de Informática da Faculdade de Ciências da Universidade de Lisboa
License     :  WTFPL

Maintainer  :  docentes-pp@listas.di.ciencias.ulisboa.pt
Stability   :  stable
Portability :  portable

Este módulo expoerta dois tipos de dados: um Ponto num espaço 2D e um Percurso, caracterizado por uma sequência de pontos.
Exporta também três funções convenientes para trabalhar com esses tipos. 

-}

module Geometria
( Ponto
, Percurso
, distancia2Pontos
, distanciaPercurso
, pontoIntermedio
) where

type Ponto = (Float, Float)

type Percurso = [Ponto]

distancia2Pontos :: Ponto -> Ponto -> Float
distancia2Pontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

pontoIntermedio :: Ponto -> Ponto -> Ponto
pontoIntermedio (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

distanciaPercurso :: Percurso -> Float
distanciaPercurso ps = sum $ map (uncurry distancia2Pontos) $ zip ps $ tail ps

