module Main where

import qualified Data.Char
import Types
import Logic
import Parsing

main :: IO ()
main = do
  s <- readFile "sudoku.txt"
  let board = readBoard s [] []
  print . show $ board
  putStrLn "Linhas adjacentes a linha 2"
  print (show (adjecentRows board 2 3))
  putStrLn "Initials"
  print (show (getRegionInitials 1 board []))
  putStrLn "Possibles reg 1"
  print (show (getRegionPossibles 1 board []))
