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
  print max
  let board = readBoard max (drop 1 s) []
  -- print board
  -- print (itopoint 7 max )
  -- print (pointToi (1, 0) max)
  -- print (adjRow 4 max board)
  -- print (adjCol 7 max board)
  -- print (inRegion 2 max board)
  -- print (possiblesAt 8 max board)
  print (solveBoard max board)

  