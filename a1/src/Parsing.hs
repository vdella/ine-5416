module Parsing where


import qualified Data.Char (digitToInt)
import Types 
import Data.Map (Map)
import qualified Data.Map as Map

readBoard :: Int -> String -> [Cell] -> [Cell] {-- Itera sobre a string (arquivo lido) e adiciona cada coluna ao tabuleiro (Board) --}
readBoard _ [] board = board
readBoard m (x:xs) board | x == '\n' = readBoard m xs board
                         | x == ' ' = readBoard m xs board
                         | otherwise = readBoard m (drop 2 xs) (board ++ [Initial (Data.Char.digitToInt x) region])
                         where region = Data.Char.digitToInt (head (drop 1 xs))
    