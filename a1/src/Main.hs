module Main where

import qualified Data.Char
import Types
import Logic
import Parsing
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  s <- readFile "suguru_board.txt"
  -- print s
  -- let board = readBoard' s 
  -- print . show $ board
  -- putStrLn "Linhas adjacentes a linha 2"
  -- print (show (adjecentRows board 2 3))
  -- putStrLn "Initials"
  -- print (show (getRegionInitials 1 board []))
  -- putStrLn "Possibles reg 1"
  -- print (show (getRegionPossibles 1 board []))

  -- print s
  print (words s)
  let newMap = readRegions' s
  print newMap
