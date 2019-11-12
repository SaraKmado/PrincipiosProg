import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)-- last 2 lines can just be getLine
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors--can be forM colors putStrLn
