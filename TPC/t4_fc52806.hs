--Sara Queimado 52806
import Geometria

--A
data Point = Tec Ponto | Inter String Ponto
data Rota = Rota {nome :: String, pontos :: [Point]}

--B
criaRota :: String -> [String] -> Percurso -> Rota
criaRota nome [] _ = Rota nome []
criaRota nome _ [] = Rota nome []
criaRota nome xs ys = Rota nome (criaRota' xs ys)

criaRota' :: [String] -> Percurso -> [Point]
criaRota' [] _ = []
criaRota' _ [] = []
criaRota' (x:xs) (y:ys) = (Inter x y) : criaRota' xs ys


--C
adicionaTecnica :: Int -> Ponto -> Rota -> Rota
adicionaTecnica pos point rota = Rota (nome rota) (add pos point (pontos rota))

add :: Int -> Ponto -> [Point] -> [Point]
add pos point points = (take pos points) ++ [(Tec point)] ++ (drop pos points)

--D

instance Show Point where
  show (Tec _) = "(Pausa)"
  show (Inter string _) = string

instance Show Rota where
  show rota = (nome rota) ++ " (" ++ (show $ round $ distanciaPercurso $ toPercurso (pontos rota)) ++ "): " ++ (toString (pontos rota))

toString :: [Point] -> String
toString []  = ""
toString (x:xs) = show x ++ foldl (\acc y -> acc ++ " --- " ++ (show y)) "" xs


toPercurso :: [Point] -> Percurso
toPercurso [] = []
toPercurso ((Tec x):xs) = (x): toPercurso xs
toPercurso ((Inter nome x):xs) = (x): toPercurso xs
