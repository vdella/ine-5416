module Main where

import qualified Data.Char
import Types
import Logic
import Parsing
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  --s <- readFile "suguru_board.txt"
  s <- readFile "sudoku.txt"
  let board = readBoard' s 
  print (show board)
  print (show (getUniqueRegions' board))
  putStrLn "-----"
  -- remove os valores impossiveis
  let newBoard = removeImpossiblesBoard (getUniqueRegions' board) board
  -- remove valores ja presentes
  let trimmedBoard =  trimBoard (getUniqueRegions' newBoard) newBoard
  putStrLn (printBoard trimmedBoard)
  
  print (checkSafeInsert board 4 3 1 2)