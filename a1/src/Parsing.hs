module Parsing where


import qualified Data.Char
import Types ( Board, Cell(Initial, Possible), Row )
import Data.Map (Map)
import qualified Data.Map as Map

readBoard' :: String -> Board
readBoard' text = readBoard text [] []

readBoard :: String -> Row -> Board -> Board {-- Itera sobre a string (arquivo lido) e adiciona cada coluna ao tabuleiro (Board) --}
readBoard [] list board = board ++ [list] {-- Fim do arquivo, adiciona o a ultima linha lida --}
readBoard (x:xs) list board {-- Nao temos mais o que ler, adiciona linha ao tabuleiro --}
  | x == '\n' = readBoard xs [] (board ++ [list]) {-- \n Marca que terminamos de ler uma linha, entao adicionamos ao tabuleiro, lemos a proxima --}
  | x == ' ' = readBoard xs list board {-- Ignora espacos --}
  | x == '0' = readBoard (drop 2 xs) (list ++ [Possible [1..9] (Data.Char.digitToInt (xs!!1))]) board {-- Marcacao de valor a ser completado, preenchemos com lista de possiveis valores --}
  | otherwise = readBoard (drop 2 xs) (list ++ [Initial (Data.Char.digitToInt x) (Data.Char.digitToInt (xs!!1))]) board {-- valor previamente fixo --}
    
    
printRow :: Row -> String -> String
printRow [] str = str
printRow ((Initial v r):xs) str = printRow xs (str++(show v)++"("++(show(r))++")"++"             ")
printRow ((Possible v r):xs) str = printRow xs (str++(show v)++"("++(show(r))++")"++"         ")

printBoard' :: Board -> String -> String 
printBoard' [] str = str
printBoard' (x:xs) str = printBoard' xs ((printRow x str)++"\n")

printBoard :: Board -> String 
printBoard board = printBoard' board ""
