module Parsing where

import qualified Data.Char
import Types ( Board, Cell(Initial, Possible), Row )
import Control.Monad ( when )
import Data.Map (Map)
import qualified Data.Map as Map

readBoard' :: String -> Board
readBoard' text = readBoard text [] []

readBoard :: String -> Row -> Board -> Board {-- Itera sobre a string (arquivo lido) e adiciona cada coluna ao tabuleiro (Board) --}
readBoard [] list board = board ++ [list] {-- Fim do arquivo, adiciona o a ultima linha lida --}
readBoard (x:xs) list board {-- Nao temos mais o que ler, adiciona linha ao tabuleiro --}
  | x == '\n' = readBoard xs [] (board ++ [list]) {-- \n Marca que terminamos de ler uma linha, entao adicionamos ao tabuleiro, lemos a proxima --}
  | x == ' ' = readBoard xs list board {-- Ignora espacos --}
  | x == '0' = readBoard xs (list ++ [Possible [1..9] 1]) board {-- Marcacao de valor a ser completado, preenchemos com lista de possiveis valores --}
  | x == '-' = board  -- Encontra a condição de parada: um carácter "-"
  | otherwise = readBoard xs (list ++ [Initial (Data.Char.digitToInt x) 1]) board {-- valor previamente fixo --}

{-- Reads the file in search of region strings to start its building.
    As the return, gives a Map, which takes the region number (an Integer) as the key
    and the region's coordinates as the value.
    
    NOTE: coordinates are given according to cartesian maps. In that way,
    the map values are lists of tuples (x, y). --}
readRegions :: Int -> [Int] -> [String] -> Map Int [Int] -> Map Int [Int]
readRegions _ _ [] filledMap = filledMap
readRegions regionNumber elements (x : xs) mapToBeFilled =
    if x == "region" then
      if regionNumber /= 0 then do
        let fillingMap = Map.insert regionNumber (tail elements) mapToBeFilled in
          readRegions (regionNumber + 1) [] xs fillingMap
      else readRegions (regionNumber + 1) [] xs mapToBeFilled
    else readRegions regionNumber (elements ++ [actualNumber]) xs mapToBeFilled where actualNumber = read x :: Int

readRegions' :: String -> Map Int [Int]
readRegions' string' = readRegions 0 [] allElements filledMap
  where filledMap = Map.empty
        allElements = words string'