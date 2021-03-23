module Parsing where


import qualified Data.Char (digitToInt)
import Types ( Cell(..), Board )
import Data.Map (Map)
import qualified Data.Map as Map

{-- Itera sobre a string (arquivo-texto a ser lido) 
    e adiciona cada coluna ao tabuleiro (Board) --}
readBoard :: Int -> String -> [Cell] -> Board
readBoard _ [] board = board
readBoard m (x:xs) board | x == '\n' = readBoard m xs board
                         | x == ' ' = readBoard m xs board
                         | otherwise = readBoard m (drop 2 xs) (board ++ [Initial (Data.Char.digitToInt x) region])
                         where region = Data.Char.digitToInt (xs !! 1)

{-- Gera um retorno visual organizado, em que usa-se os valores das células, 
    e, entre parênteses, o valor de suas regiões. --}
printBoard :: Board -> Int -> String
printBoard board max = printBoard' 0 max board
    where 
        printBoard' _ _ [] = "\n"
        printBoard' i max ((Initial v r):ys) 
            | i == max = "\n" ++ printBoard' 0 max (Initial v r:ys)
            | otherwise = show v ++ "(" ++ show r ++ ")   " ++ printBoard' (i+1) max ys
