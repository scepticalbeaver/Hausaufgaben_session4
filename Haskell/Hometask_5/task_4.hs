module Task4 where

import Control.Monad

main = 
        putStr "Enter n - max Int in list:\n> " >>
        readLn >>= \n ->
        putStrLn $ show (map (\ab -> (ab!!0) * (ab!!1)) (replicateM 2 [1..n]))





