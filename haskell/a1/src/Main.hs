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
  {-- A primeira linha do arquivo-texto denota quantas linhas há na matriz. --}
  let max = Data.Char.digitToInt (head s)
  let board = readBoard max (drop 1 s) []  -- Tira-se a linha 1 do arquivo-texto: passa-se somente a matriz.
  putStrLn "Tabuleiro original: "
  putStrLn (printBoard board max)  -- Tabuleiro não alterado.
  let solvedBoard = solveBoard max board
  putStrLn "Tabuleiro resolvido: "
  putStrLn (printBoard solvedBoard max)  -- Tabuleiro resolvido.

  -- print (length solvedBoard)
  
