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
  -- print (show (board!!0))
  -- let regionInitials = getRegionInitials 2 board []
  -- let regionInitialsValues = getInitialsValues regionInitials []
  -- print (trimBoard regionInitialsValues 2 board [])
  -- print (valueInRegion board 1 2)
  print (show (trimBoard (getUniqueRegions' board) board))

  -- print . show $ board
  -- putStrLn "Linhas adjacentes a linha 2"
  -- print (show (adjecentRows board 2 3))
  -- putStrLn "Initials"
  -- print (show (getRegionInitials 1 board []))
  -- putStrLn "Possibles reg 1"
  -- print (show (getRegionPossibles 1 board []))

  -- print s
  -- print (words s)
  -- let newMap = readRegions' s
  -- print newMap
