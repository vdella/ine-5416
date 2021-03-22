module Backtracking where

import Types ( Board, Cell(Initial, Possible), Row )
import Logic
    ( checkSafeInsert,
      countLines,
      insertCellBoard,
      removeFromPossible )

solve :: Board -> Board
solve board = solveBoard' board (head board) 1 0 board

solveBoard' :: Board -> Row -> Int -> Int -> Board -> Board
solveBoard' board [] _ _ _ = board
solveBoard' board ((Possible [] _):ys) i j alternativeBoard | j > 0 = solveBoard' (updatedBoard i (j-1)) (drop (j-1) (updatedBoard i (j-1)!!i)) i (j-1) alternativeBoard -- caso nao haja mais opcoes para colocar no tabuleiro, voltamos para o estado anterior (possivelmente errado esse segundo alternative board)
                                                            | otherwise = solveBoard' (updatedBoard (i-1) 0) (updatedBoard (i-1) 0!!(i-1)) (i-1) 0 alternativeBoard
                                                            where
                                                                updatedBoard x y = insertCellBoard board (alternativeBoard!!x!!y) x y []

solveBoard' board ((Possible (v:vs) r):ys) i j alternativeBoard
    | checkSafeInsert board v i j = if j < countLines board - 1 then
                                        solveBoard' (insertCellBoard board (Initial v r) i j []) ys i (j+1) (insertCellBoard board (removeFromPossible (board!!i!!j) v) i j []) -- inserimos e salvamos o board alternativo com os outros valores possiveis
                                    else solveBoard' (insertCellBoard board (Initial v r) i j []) (getHeadFrom (drop (i + 1) board)) (i + 1) 0 (insertCellBoard board (removeFromPossible (board!!i!!j) v) i j []) -- mesma coisa mas pulamos a linha
    | otherwise = solveBoard' board (Possible vs r:ys) i j alternativeBoard -- nao podemos inserir, tentamos com o proximo valor possivel

solveBoard' board ((Initial v r):xs) i j alternativeBoard = if j < countLines board - 1 then
                                                                solveBoard' board xs i (j + 1) alternativeBoard  -- Jumps to the next column, as we increment j.
                                                            else solveBoard' board (getHeadFrom (drop (i + 1) board)) (i + 1) 0 alternativeBoard

getHeadFrom :: Board -> Row
getHeadFrom list = if countLines list /= 0 then head list else []
