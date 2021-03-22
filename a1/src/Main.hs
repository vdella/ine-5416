module Main where

import qualified Data.Char
import Types
import Logic
import Parsing
import Data.Map (Map)
import qualified Data.Map as Map

import Backtracking ( solveBoard ) 

main :: IO ()
main = do
  --s <- readFile "suguru_board.txt"
  s <- readFile "sudoku.txt"
  let board = readBoard' s 
  -- print (show board)
  -- print (show (getUniqueRegions' board))
  -- putStrLn "-----"
  -- remove os valores impossiveis
  let newBoard = removeImpossiblesBoard (getUniqueRegions' board) board
  -- putStrLn (printBoard newBoard)
  -- remove valores ja presentes
  let trimmedBoard =  trimBoard (getUniqueRegions' newBoard) newBoard
  -- putStrLn (printBoard trimmedBoard)

  -- let solvedRow = solveRow board (board!!0) (adjacentRows board 0) 0 0 [] []
  
  -- print solvedRow
  -- print (solveRow ([solvedRow] ++ (drop 1 trimmedBoard)) (trimmedBoard!!1) (adjacentRows trimmedBoard 1) 0 0 [] [])

  -- print (solveBoard trimmedBoard)
  -- Is the infinite loop down here?
  putStrLn (printBoard (solveBoard trimmedBoard (trimmedBoard!!0) 0 0 trimmedBoard))