module Logic where

import Types
import Debug.Trace

-- converte um indice i em um par (i,j) indicando linha e coluna, repectivamente, na 'matriz' de valores
itopoint :: Int -> Int -> (Int, Int)
itopoint i max = (calcX i, calcY i)
    where calcY i = i - max * div i max
          calcX i = div i max
          
-- converte um ponto (i,j) da 'matriz' de valores em um índice na lista
pointToi :: (Int, Int) -> Int -> Int
pointToi (i, j) max = j + i * max

-- retorna as células adjacentes ao índice i que se encontram na mesma linha
adjRow :: Int -> Int -> Board -> [Cell]
adjRow i max board = helper (itopoint i max) -- converte o indice para um ponto (i,j) para sabermos quais os vizinhos relativos
    where helper (i, j) | (j-1) < 0 = [board !! pointToi (i, j+1) max]
                        | (j+1) >= max = [board !! pointToi (i, j-1) max]
                        | otherwise = [board !! pointToi (i, j+1) max] ++ [board !! pointToi (i, j-1) max]


-- retorna as células adjacentes ao índice i que se encontram na mesma coluna e nas diagonais
adjCol :: Int -> Int -> Board -> [Cell]
adjCol i max board = helper (itopoint i max) -- converte o indice para um ponto (i,j) para sabermos quais os vizinhos relativos
    {-- aqui precisamos fazer um bounds-check para que temos certeza que os valores sao imediatamente adjacentes, para isso
     temos que considerar que tanto i como j podem estar no 'limite' da matriz ou mesmo que ambos estejam --}
    where helper (i, j) | (i-1) < 0 && (j+1) > (max-1) = [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j-1) max]
                        | (i-1) < 0 && (j-1) < 0 = [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j+1) max]
                        | (i-1) < 0 = [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j+1) max] ++ [board !! pointToi (i+1, j-1) max]
                        | (i+1) > (max-1) && (j+1) > (max-1) = [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j-1) max]
                        | (i+1) > (max-1) && (j-1) < 0 = [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j+1) max]
                        | (i+1) > (max-1) = [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j+1) max] ++ [board !! pointToi (i-1, j-1) max]
                        | (j-1) < 0 = [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j+1) max] ++ [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j+1) max]
                        | (j+1) > (max-1) = [board !! pointToi (i+1, j) max] ++ [board !! pointToi (i+1, j-1) max] ++ [board !! pointToi (i-1, j) max] ++ [board !! pointToi (i-1, j-1) max]
                        | otherwise = [board !! pointToi (i+1, j) max] ++ 
                                      [board !! pointToi (i-1, j) max] ++ 
                                      [board !! pointToi (i+1, j+1) max] ++ 
                                      [board !! pointToi (i+1, j-1) max] ++
                                      [board !! pointToi (i-1, j+1) max] ++ 
                                      [board !! pointToi (i-1, j-1) max]

-- dada uma lista celulas, retornamos uma lista com seus valores correspondentes
cellsToVals :: [Cell] -> [Int]
cellsToVals [] = []
cellsToVals ((Initial v r):xs) = [v] ++ cellsToVals xs
                                      
-- dada uma celula, retornamos a sua regiao
cellRegion :: Cell -> Int
cellRegion (Initial _ r) = r

-- dada uma celula, retornamos o seu valor
cellValue :: Cell -> Int
cellValue (Initial v _) = v

-- retorna uma lista de celulas pertencentes a regiao de entrada
inRegion :: Int -> Int -> Board -> [Cell] 
inRegion _ _ [] = []
inRegion region max ((Initial v r):xs) | region == r = [Initial v r] ++ inRegion region max xs
                                       | otherwise = inRegion region max xs

-- retorna o tamanho da regiao de entrada (quantidade de celulas)
regionLength :: Int -> Int -> Board -> Int
regionLength region max board = length (inRegion region max board)
                                       
-- remove da primeira lista os valores presentes na segunda lista
removeVal :: [Int] -> [Int] -> [Int]
removeVal [] _ = []
removeVal xs [] = xs
removeVal xs (y:ys) = removeVal (removeAll y xs) ys

-- remove da lista todas as ocorrencias de um dado valor 
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll y (x:xs) | x == y = removeAll y xs
                   | otherwise = [x] ++ removeAll y xs
                   
-- retorna todos os valores que são possíveis de inserir no indice de entrada
possiblesAt :: Int -> Int -> Board -> [Int]
possiblesAt i max board | i > length board = []
                    | cellValue( (board !! i)) == 0 = [1..rmax] `removeVal` (cellsToVals ((adjRow i max board) ++ (adjCol i max board) ++ ( inRegion (cellRegion (board!!i)) max board)))
                    | otherwise = [cellValue (board !! i)]
                    where rmax = regionLength (cellRegion (board !! i)) max board
                    
-- insere a celula dada como entrada no tabuleiro
tryInsert :: Int -> Board -> Cell -> Board
tryInsert i board cell = take i board ++ [cell] ++ drop (i + 1) board


-- retorna o indice correspondente a proxima célula onde se é possível inserir um valor
nextPossible :: Int -> Board -> Int
nextPossible i board | i == length board -1 = length board -1
                  | cellValue (board !! (i + 1)) == 0 = i + 1
                  | otherwise = nextPossible (i + 1) board
                  
{-- itera sobre as celulas do tabuleiro tentando inserir um valor da lista de possiveis valores. Caso falhe em encontrar valores disponiveis,
tenta novamente com o proximo valor possivel ate resolver o tabuleiro --}
solve :: Int -> Int -> Board -> [Int] -> Board
solve 24 max board [] = []
solve 24 max board (x:[]) = tryInsert 24 board (Initial x (cellRegion (board!!24)))
solve 24 max board (x:_) = []
solve _ max board [] = []
solve i max board (x:xs) | solvedAhead == [] = solve i max board xs
                         | otherwise = solvedAhead
                         where solveNext i board = solve (nextPossible i board) max board (possiblesAt (nextPossible i board) max board)
                               solvedAhead = solveNext i (tryInsert i board (Initial x region))
                               region = cellRegion (board!!i)
                               
-- chama a funcao que resolve o tabuleiro
solveBoard :: Int -> Board -> Board
solveBoard max board = solve 0 max board (possiblesAt 0 max board)