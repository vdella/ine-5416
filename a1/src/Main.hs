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
  let max = Data.Char.digitToInt (head s)
  let board = readBoard max (drop 1 s) []
  putStrLn (printBoard board max)
  let solvedBoard = solveBoard max board
  putStrLn (printBoard solvedBoard max)

  