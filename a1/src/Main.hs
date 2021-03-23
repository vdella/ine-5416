module Main where

import qualified Data.Char
import Types ()
import Logic ( solveBoard )
import Parsing ( printBoard, readBoard )
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  s <- readFile "sudoku.txt"
  let max = Data.Char.digitToInt (head s)
  let board = readBoard max (drop 1 s) []
  putStrLn "Tabuleiro original: "
  putStrLn (printBoard board max)
  let solvedBoard = solveBoard max board
  putStrLn "Tabuleiro resolvido: "
  putStrLn (printBoard solvedBoard max)

  
