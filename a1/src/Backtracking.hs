module Backtracking where

import Types
import Logic


{-- tabuleiro, linha em que se comeca o solve, 0, 0, copia do tabuleiro--}
solveBoard :: Board -> Row -> Int -> Int -> Board -> Board
solveBoard board [] _ _ _ = board
solveBoard board ((Possible [] _):ys) i j alternativeBoard | j > 0 = solveBoard (updatedBoard i (j-1)) (drop (j-1) (updatedBoard i (j-1)!!i)) i (j-1) alternativeBoard -- caso nao haja mais opcoes para colocar no tabuleiro, voltamos para o estado anterior (possivelemnte errado esse segundo alternative board)
                                                           | otherwise = solveBoard (updatedBoard (i-1) 0) (updatedBoard (i-1) 0!!(i-1)) (i-1) 0 alternativeBoard
                                                            where
                                                                updatedBoard x y = insertCellBoard board (alternativeBoard!!x!!y) x y []
solveBoard board ((Possible (v:vs) r):ys) i j alternativeBoard | checkSafeInsert board v i j = if j < countLines board -1 -- checa se podemos inserir
                                                                                                then solveBoard (insertCellBoard board (Initial v r) i j []) ys i (j+1) (insertCellBoard board (removeFromPossible (board!!i!!j) v) i j []) -- inserimos e salvamos o board alternativo com os outros valores possiveis
                                                                                                else solveBoard (insertCellBoard board (Initial v r) i j []) (primeiro (drop (i+1) board)) (i+1) 0 (insertCellBoard board (removeFromPossible (board!!i!!j) v) i j []) -- mesma coisa mas pulamos a linha
                                                               | otherwise = solveBoard board (Possible vs r:ys) i j alternativeBoard -- nao podemos inserir, tentamos com o proximo valor possivel
                                                                where
                                                                    primeiro xs | countLines xs == 0 = []
                                                                                | otherwise = head xs
solveBoard board ((Initial v r):xs) i j alternativeBoard | j < countLines board -1 = solveBoard board xs i (j+1) alternativeBoard -- pulamos initials
                                                         | otherwise = solveBoard board (primeiro (drop (i+1) board)) (i+1) 0 alternativeBoard
                                                         where
                                                             primeiro xs | countLines xs == 0 = []
                                                                         | otherwise = head xs

                                                                         -- {-- board, linha a ser resolvida, linhas adjacentes, x-inicio, y-inicio, vazio, vazio --}
-- {-- board, linha a ser resolvida, linhas adjacentes, x-inicio, y-inicio, vazio, vazio --}
-- solveRow :: Board -> Row -> [Row] -> Int -> Int -> Board -> Board -> Row
-- solveRow board [] _ _ _ newBoard _ = newBoard
-- solveRow board ((Initial v r):xs) adjacents i j newRow previousState | j < countLines board -1 = solveRow board xs adjacents i (j+1) (newRow++[Initial v r]) (previousState++[Initial v r])
--                                                                      | otherwise = solveRow board xs adjacents (i+1) 0 (newRow++[Initial v r]) (previousState++[Initial v r])
-- solveRow board ((Possible (v:vs) r):xs) adjacents i j newRow previousState | j < countLines board -1 = if checkSafeInsert board v i j
--                                                                                                         then solveRow (insertValueBoard board v r i j []) xs adjacents i (j+1) (newRow++[Initial v r]) (previousState++[Possible vs r])
--                                                                                                         else solveRow board (Possible vs r : xs) adjacents i j newRow previousState
--                                                                            | otherwise = if checkSafeInsert board v i j
--                                                                                                         then newRow++[Initial v r] 
--                                                                                                         else solveRow board (Possible vs r : xs) adjacents i j newRow previousState