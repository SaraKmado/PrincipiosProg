import System.Random

threeCoins :: StdGen -> (Bool,Bool,Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

d6 :: Int -> Int
d6 n = fst $ randomR (1,6) (mkStdGen n)

d12 :: Int -> Int
d12 n = (randomRs (1,12) (mkStdGen n) :: [Int]) !! 120
