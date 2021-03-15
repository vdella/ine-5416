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
  putStrLn (printBoard newBoard)
  -- remove valores ja presentes
  let trimmedBoard =  trimBoard (getUniqueRegions' newBoard) newBoard
  putStrLn (printBoard trimmedBoard)

  print (solveRow board (board!!0) (adjacentRows board 0) 0 0 [] [])
