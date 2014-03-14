module Task_5 where

import SortedList

doLoop :: [[Char]] -> IO [[Char]]
doLoop list = do
    putStr "\nEnter the number of command: \n"
    putStr "0 - exit\n"
    putStr "1 - add value to sorted list\n"
    putStr "2 - remove value from list\n"
    putStr "3 - print list\n> "

    command <- getLine
    case command of
        '0' : _ -> return list
        '1': _ -> do
                    putStr "put > "
                    n <- getLine
                    doLoop $ add n list
        '2' : _ -> do 
                    putStr "value to remove > "
                    n <- getLine
                    doLoop $ remove n list
        '3' : _ -> do
                    putStrLn (show(list))
                    doLoop list
        _ -> do doLoop list

main = do
    doLoop []