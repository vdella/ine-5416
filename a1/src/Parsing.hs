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
    


-- {-- Reads the file in search of region strings to start its building.
--     As the return, gives a Map, which takes the region number (an Integer) as the key
--     and the region's coordinates as the value.

--     !!!FIXME: LAST REGION NOT PUT INSIDE THE MAP. ONLY GETS THE PREVIOUS REGIONS, NOT THE LAST.
-- --}
-- readRegions :: Int -> [Int] -> [String] -> Map Int [Int] -> Map Int [Int]
-- readRegions _ _ [] filledMap = filledMap  -- Being with empty [String], gives a new map back.
-- readRegions regionNumber elements (x : xs) mapToBeFilled =
--     if x == "region" then 
--       if regionNumber /= 0 then do

--         -- We have to insert the list of numbers of a given region inside the map right
--         -- at the last second: when we are about to start parsing some other new region!

--         let fillingMap = Map.insert regionNumber (tail elements) mapToBeFilled in  -- Use "tail elements" to evade using the proper region number
--           readRegions (regionNumber + 1) [] xs fillingMap                          -- such as "region 1". We want only what comes after that.

--       else readRegions (regionNumber + 1) [] xs mapToBeFilled  -- Increment region index and go on!
--     -- Append found number to list and parse the rest, then.
--     else readRegions regionNumber (elements ++ [actualNumber]) xs mapToBeFilled where actualNumber = read x :: Int

-- readRegions' :: String -> Map Int [Int]  -- Used during execution. Hides interns underneath.
-- readRegions' string' = readRegions 0 [] allElements filledMap
--   where filledMap = Map.empty
--         allElements = words string'