module Main where

import qualified Data.Char
import Types
import Logic
import Parsing
import Data.Map (Map)
import qualified Data.Map as Map

import Backtracking ( solve )

main :: IO ()
main = do
  s <- readFile "sudoku.txt"
  let board = readBoard' s
  let newBoard = removeImpossiblesBoard (getUniqueRegions' board) board
  let trimmedBoard =  trimBoard (getUniqueRegions' newBoard) newBoard
  putStrLn (printBoard (solve trimmedBoard))
