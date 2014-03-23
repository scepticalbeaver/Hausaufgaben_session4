module Task5 where

doFindfstLocalMax :: [Int] -> IO()
doFindfstLocalMax xs = dofindHelper (minBound :: Int) xs
    where
    dofindHelper :: (Num a, Ord a, Show a) => a -> [a] -> IO()
    dofindHelper prev [curr] = if curr > prev
                                then printResult curr
                                else return ()
    dofindHelper prev (x : xs) = if ((x > prev) && (x > (head xs)))
                                then printResult x
                                else dofindHelper x xs
    printResult x = putStrLn $ "local max: " ++ (show x)


main = do
        putStr "input some list here (e.g. [1,2,3]:\n >"
        list <- getLine
        doFindfstLocalMax (read list)