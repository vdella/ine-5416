module Logic where

import Types ( Cell(..), Board )
import Debug.Trace ()

-- Converte um índice i em um par (i,j), indicando linha e coluna, respectivamente, no tabuleiro.
itopoint :: Int -> Int -> (Int, Int)
itopoint i max = (calcX i, calcY i)
    where calcY i = i - max * div i max
          calcX i = div i max

-- Converte um ponto (i,j) do tabuleiro em um índice na lista.
pointToi :: (Int, Int) -> Int -> Int
pointToi (i, j) max = j + i * max

-- Retorna as células adjacentes ao índice i que se encontram na mesma linha.
adjRow :: Int -> Int -> Board -> [Cell]
adjRow i max board = helper (itopoint i max) -- Converte o índice para um ponto (i,j) para sabermos quais os vizinhos relativos.
    where helper (i, j) | j-1 < 0 = [board !! pointToi (i, j+1) max]
                        | j+1 >= max = [board !! pointToi (i, j-1) max]
                        | otherwise = (board !! pointToi (i, j+1) max) : [board !! pointToi (i, j-1) max]


-- Retorna as células adjacentes ao índice i que se encontram na mesma coluna e nas diagonais
adjCol :: Int -> Int -> Board -> [Cell]
adjCol i max board = helper (itopoint i max) -- Converte o índice i para um ponto (i,j) para que se saiba os vizinhos relativos.

    {-- Faz-se um bound-check para que se tenha certeza de que os valores são imediatamente adjacentes; para isto
     é necessário verificar se i ou j possuem valores equivalentes aos dos limites do tabuleiro: 0 ou lenght - 1.
    --}
    where helper (i, j) | i-1 < 0 && j+1 > max-1 = 
                            (board !! pointToi (i+1, j) max) : [board !! pointToi (i+1, j-1) max]
                        | i-1 < 0 && j-1 < 0 = 
                            (board !! pointToi (i+1, j) max) : [board !! pointToi (i+1, j+1) max]
                        | i-1 < 0 = 
                            [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j+1) max] ++ [board !! pointToi (i+1, j-1) max]
                        | i+1 > max-1 && j+1 > max-1 = 
                            (board !! pointToi (i-1, j) max) : [board !! pointToi (i-1, j-1) max]
                        | i+1 > max-1 && j-1 < 0 = 
                            (board !! pointToi (i-1, j) max) : [board !! pointToi (i-1, j+1) max]
                        | i+1 > max-1 = 
                            [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j+1) max] ++ [board !! pointToi (i-1, j-1) max]
                        | j-1 < 0 = 
                            [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j+1) max] ++ [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j+1) max]
                        | j+1 > max-1 = 
                            [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j-1) max] ++ [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j-1) max]
                        | otherwise = 
                            [board !! pointToi (i+1, j) max]   ++
                            [board !! pointToi (i-1, j) max]   ++
                            [board !! pointToi (i+1, j+1) max] ++
                            [board !! pointToi (i+1, j-1) max] ++
                            [board !! pointToi (i-1, j+1) max] ++
                            [board !! pointToi (i-1, j-1) max]

-- Dada uma lista de células, retorna-se uma lista com seus valores correspondentes.
cellsToVals :: [Cell] -> [Int]
cellsToVals [] = []
cellsToVals ((Initial v r):xs) = v : cellsToVals xs

-- Retorna-se a região de uma dada célula.
cellRegion :: Cell -> Int
cellRegion (Initial _ r) = r

-- Retorna-se o valor de uma célula.
cellValue :: Cell -> Int
cellValue (Initial v _) = v

-- Retorna uma lista de células pertencentes à região de entrada.
inRegion :: Int -> Int -> Board -> [Cell]
inRegion _ _ [] = []
inRegion region max ((Initial v r):xs) 
    | region == r = Initial v r : inRegion region max xs
    | otherwise = inRegion region max xs

-- Retorna o tamanho da região de entrada como o seu tamanho em células.
regionLength :: Int -> Int -> Board -> Int
regionLength region max board = length (inRegion region max board)

-- Remove da primeira lista os valores presentes na segunda lista.
removeVal :: [Int] -> [Int] -> [Int]
removeVal [] _ = []
removeVal xs [] = xs
removeVal xs (y:ys) = removeVal (removeAll y xs) ys

-- Remove da lista todas as ocorr6encias de um dado valor.
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll y (x:xs) | x == y = removeAll y xs
                   | otherwise = x : removeAll y xs

-- Retorna todos os valores que são possíveis a serem inseridos no índice de entrada.
possiblesAt :: Int -> Int -> Board -> [Int]
possiblesAt i max board 
    | i > length board = []
    | cellValue( board !! i) == 0 = [1..rmax] `removeVal` cellsToVals (adjRow i max board ++ adjCol i max board ++ inRegion (cellRegion (board!!i)) max board)
    | otherwise = [cellValue (board !! i)]
    where rmax = regionLength (cellRegion (board !! i)) max board

-- Insere a célula dada como entrada no tabuleiro.
tryInsert :: Int -> Board -> Cell -> Board
tryInsert i board cell = take i board ++ [cell] ++ drop (i + 1) board


-- Retorna o índice correspondente à próxima célula em que é possível inserir um valor.
nextPossible :: Int -> Board -> Int
nextPossible i board | i == length board -1 = length board -1
                  | cellValue (board !! (i + 1)) == 0 = i + 1
                  | otherwise = nextPossible (i + 1) board

{-- Itera sobre as células do tabuleiro tentando inserir um valor da lista de possíveis valores. Caso falhe em encontrar valores disponíveis,
    tenta novamente com o próximo valor possível até resolver o tabuleiro. --}
solve :: Int -> Int -> Board -> [Int] -> Board
solve _ max board [] = []
solve i max board (x:xs) | i == length board -1 && null (x:xs) = []
                         | i == length board -1 && null xs = tryInsert i board (Initial x (cellRegion (board!!i)))
                         | i == length board -1 && not (null [x]) = []
                         | null solvedAhead = solve i max board xs
                         | otherwise = solvedAhead
                         where solveNext i board = solve (nextPossible i board) max board (possiblesAt (nextPossible i board) max board)
                               solvedAhead = solveNext i (tryInsert i board (Initial x region))
                               region = cellRegion (board!!i)

-- Chama a função responsável pela resolução do tabuleiro.
solveBoard :: Int -> Board -> Board
solveBoard max board = solve 0 max board (possiblesAt 0 max board)
