module Logic where

import Types

{-- dado o tabuleiro, indice e max de linhas, devolve as linhas adjacentes  a linha indice --}
adjacentRows :: Board -> Int -> [Row]
adjacentRows board i  | i == 0 = [board!!1]
                      | i == countLines board  -1 = [board!!(i-1)]  
                      | i >= countLines board -1 = []
                      | otherwise = [board!!(i-1)] ++ [board!!(i+1)]


{-- dado uma linha adjacente e o indice da celula original e o max de linhas, retorna as celulas adjacentes a essa celula (diagonais inclusas)--}
adjacentCellsValues :: Row -> Int -> Int -> [Value]
adjacentCellsValues [] _ _ = []
adjacentCellsValues row i max | i == 0 = [cellValue (row!!i)] ++  [cellValue (row!!(i+1))]
                              | i == (max - 1) = [cellValue (row!!(i-1))] ++ [cellValue (row!!i)]
                              | otherwise = [cellValue (row!!(i-1))] ++ [cellValue(row!!i)] ++ [cellValue (row!!(i+1))]

adjacentCellsValues' :: [Row] -> Int -> Int -> [Value]
adjacentCellsValues' [] _ _ = []
adjacentCellsValues' (x:xs) i max = (adjacentCellsValues x i max) ++ (adjacentCellsValues' xs i max)

cellValue :: Cell -> Int 
cellValue (Possible (x:xs) _) = 0
cellValue (Initial v _) = v

{-- dado uma linha, uma lista para armazenar os Initial e uma regiao, retorna os valores Initial --}
getRowInitials :: Row -> [Cell] -> Int -> [Cell]
getRowInitials [] initials _ = initials
getRowInitials ((Possible ys r):xs) initials reg = getRowInitials xs initials reg
getRowInitials ((Initial x r):xs) initials reg | r == reg = getRowInitials xs (initials ++ [Initial x r]) reg
                                               | otherwise = getRowInitials xs initials reg


{-- dado uma regiao, o tabuleiro e uma lista para armazenar os valores, retorna os valores iniciais desta regiao --}
getRegionInitials :: Int -> Board -> [Cell] -> [Cell]
getRegionInitials _ [] initials = initials
getRegionInitials r (x:xs) initials = getRowInitials x initials r ++ getRegionInitials r xs initials

getInitialsValues :: [Cell] -> [Int] -> [Int]
getInitialsValues [] list = list
getInitialsValues ((Initial x r):xs) list = getInitialsValues xs (list ++ [x])

{-- dado uma linha, uma lista para armazenar os Possible e uma regiao, retorna os valores Possible --}
getRowPossibles :: Row -> [Cell] -> Int -> [Cell]
getRowPossibles [] possibles _ = possibles
getRowPossibles ((Initial y r):xs) possibles reg = getRowPossibles xs possibles reg
getRowPossibles ((Possible ys r):xs) possibles reg | r == reg = getRowPossibles xs (possibles ++ [Possible ys r]) reg
                                                  | otherwise = getRowPossibles xs possibles reg


{-- dado uma regiao, o tabuleiro e uma lista para armazenar os valores, retorna os valores iniciais desta regiao --}
getRegionPossibles :: Int -> Board -> [Cell] -> [Cell]
getRegionPossibles _ [] possibles = possibles
getRegionPossibles r (x:xs) possibles = getRowPossibles x possibles r ++ getRegionPossibles r xs possibles

getUniqueRegions' :: Board -> [Int]
getUniqueRegions' board = getUniqueRegions board []

{-- retorna as regioes que existem no tabuleiro --}
{-- como temos uma lista de listas, o algoritmo compara se a regiao do primeiro item da primeira lista esta na lista de regioes,
se sim, ele passa para o proximo item, se nao ele adiciona la lista de regioes. Quando esgotamos a primeira lista, passamos para a proxima--}
getUniqueRegions :: Board -> [Int] -> [Int]
getUniqueRegions [] list = list
getUniqueRegions ([]:xs) list = getUniqueRegions xs list -- pula a para a proxima linha do tabuleiro
getUniqueRegions (((Possible _ region):ys):xs) list = if not (contains list region) then getUniqueRegions (ys:xs) (list ++ [region]) else getUniqueRegions (ys:xs) list 
getUniqueRegions (((Initial _ region):ys):xs) list = if not (contains list region) then getUniqueRegions (ys:xs) (list ++ [region]) else getUniqueRegions (ys:xs) list

{-- dada uma linha, r   emove uma lista de valores dos valores possiveis de uma regiao --}
{-- Valores a serem tirados, regiao, linha, vazio--}
trimRegion :: [Value] -> Region -> Row -> Row -> Row
trimRegion _ _ [] newRow = newRow
trimRegion vals reg ((Possible ys r):xs) newRow | reg == r = trimRegion vals reg xs (newRow ++ [checkSingle (removeValues vals ys) r]) -- se for a regiao alvo, remove o valor da lista de Possible
                                                | otherwise = trimRegion vals reg xs (newRow ++ [Possible ys r]) -- caso contrario nao mudamos nada
                                               where
                                                   removeValues [] ys = ys
                                                   removeValues (x:xs) ys = removeValues xs (removeItem x ys)
                                                   checkSingle xs reg = if length xs == 1 then Initial (head xs) r else Possible xs r -- se a lista contiver apenas um elemento, retornamos um Initial
trimRegion val reg ((Initial v r):xs) newRow = trimRegion val reg xs (newRow ++ [Initial v r]) -- se for um valor ja certo, nao fazemos modificacao

{-- Dado uma lista de valores, uma regiao e o tabuleiro, retira todos os valores contido na lista dos valores possiveis desta regiao de todo o tabuleiro--}
trimBoard' :: [Value] -> Region -> Board -> Board -> Board
trimBoard' _ _ [] newBoard = newBoard
trimBoard' vals reg (x:xs) newBoard = trimBoard' vals reg xs (newBoard ++ [trimRegion vals reg x []])

{-- Dado uma lista contendo todas as regioes unicas do tabuleiro, e o proprio tabuleiro, remove os valores repetidos de cada Possible de cada regiao --}
trimBoard :: [Region] -> Board -> Board 
trimBoard [] board = board
trimBoard (x:xs) board = trimBoard xs (trimBoard' (getInitialsValues (getRegionInitials x board []) []) x board [])

{-- dada uma linha, checa se o valor val esta na regiao reg --}
valueInRegionRow :: Row -> Value -> Region -> Bool
valueInRegionRow [] _ _ = False 
valueInRegionRow ((Initial v r):xs) val reg | r == reg = v == val || valueInRegionRow xs val reg
                                            | otherwise = valueInRegionRow xs val reg
valueInRegionRow ((Possible _ _):xs) val reg = valueInRegionRow xs val reg

{-- dado o tabuleiro, checa se o valor val esta na regiao reg --}
valueInRegion :: Board -> Value -> Region -> Bool
valueInRegion [] _ _ = False 
valueInRegion (x:xs) val reg = valueInRegionRow x val reg || valueInRegion xs val reg


removeItem :: Int -> [Int] -> [Int]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

contains :: [Int] -> Int -> Bool
contains [] _ = False
contains (x:xs) i 
    | x == i = True
    | otherwise = contains xs i
    

countLines :: Board -> Int 
countLines board = countLines' board 0

countLines' :: Board -> Int -> Int 
countLines' [] num = num
countLines' (x:xs) num = countLines' xs (num + 1)

{-- dada uma linha, retorna a quantidade de celulas de uma certa regiao  --}
regionCellsCountRow :: Region -> Row -> Int -> Int
regionCellsCountRow reg [] qnt = qnt
regionCellsCountRow reg ((Initial v r):xs) qnt | reg == r = regionCellsCountRow reg xs (qnt+1)
                                             | otherwise = regionCellsCountRow reg xs qnt
regionCellsCountRow reg ((Possible v r):xs) qnt | reg == r = regionCellsCountRow reg xs (qnt+1)
                                              | otherwise = regionCellsCountRow reg xs qnt
                                              
regionCellsCountBoard' :: Region -> Board -> Int -> Int
regionCellsCountBoard' reg [] qnt = qnt
regionCellsCountBoard' reg (x:xs) qnt = regionCellsCountRow reg x qnt + regionCellsCountBoard' reg xs qnt

{-- dada o tabuleiro, retorna a quantidade de celulas de uma certa regiao  --}
regionCellsCountBoard :: Region -> Board -> Int
regionCellsCountBoard reg board = regionCellsCountBoard' reg board 0

{-- TODO: adicionar funcao que retira os valores Possible que sao impossiveis. Ex: se uma regia tem 4 celular , os valores possiveis sao 1..4 e nao 1..9 como padrao --}
{-- talvez especificar isso no arquivo e poupar de fazer funcao? --}

{-- dada uma regiao e a quantidade de celulas desta regiao, ajusta a lista de possiveis valores dessa regiao --}
removeImpossiblesRow :: Region -> Int -> Row -> Row -> Row
removeImpossiblesRow _ _ [] newRow = newRow
removeImpossiblesRow reg qnt ((Initial v r):xs) newRow = removeImpossiblesRow reg qnt xs (newRow ++ [Initial v r])
removeImpossiblesRow reg qnt ((Possible v r):xs) newRow  | reg == r = removeImpossiblesRow reg qnt xs (newRow ++ [Possible [1..qnt] r])
                                                         | otherwise = removeImpossiblesRow reg qnt xs (newRow ++ [Possible v r])

removeImpossiblesBoard' :: Region -> Int -> Board -> Board -> Board 
removeImpossiblesBoard' _ _ [] newBoard = newBoard
removeImpossiblesBoard' reg qnt (x:xs) newBoard = removeImpossiblesBoard' reg qnt xs (newBoard ++ [removeImpossiblesRow reg qnt x []])

removeImpossiblesBoard :: [Region] -> Board -> Board
removeImpossiblesBoard [x] board = removeImpossiblesBoard' x (regionCellsCountBoard x board) board []
removeImpossiblesBoard (x:xs) board = removeImpossiblesBoard xs (removeImpossiblesBoard' x (regionCellsCountBoard x board) board [])



{-- tabuleiro, valor, regiao, x, y--}
checkSafeInsert :: Board -> Value -> Int -> Int -> Bool
checkSafeInsert board val x y = not (valueInRegion board val (getRegion board x y)) && not (contains (adjacentCellsValues' (adjacentRows board x) y (countLines board)) val) && sameRowCheck
                                    where
                                        sameRowCheck  | y == 0 = cellValue (board!!x!!(y+1)) /= val 
                                                      | y == countLines board -1 = cellValue (board!!x!!(y-1)) /= val
                                                      | otherwise = cellValue (board!!x!!(y-1)) /= val && cellValue (board!!x!!(y+1)) /= val


{-- dado o tabuleiro e as coordenadas x e y, retorna a regiao da celula --}
getRegion :: Board -> Int -> Int -> Region
getRegion board x y = getRegion' (board!!x!!y)

getRegion' :: Cell -> Int 
getRegion' (Initial _ r) = r
getRegion' (Possible _ r) = r

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

{-- tabuleiro, linha em que se comeca o solve, 0, 0, copia do tabuleiro--}                                                                                                            
solveBoard :: Board -> Row -> Int -> Int -> Board -> Board
solveBoard board [] _ _ _ = board
solveBoard board ((Possible (v:vs) r):ys) i j alternativeBoard | j < countLines board -1 = if checkSafeInsert board v i j
                                                                                                        then solveBoard (insertCellBoard board (Initial v r) i j []) ys i (j+1) (insertCellBoard board (Possible vs r) i j [])
                                                                                                        else solveBoard alternativeBoard ((Possible (vs) r):ys) i j alternativeBoard
                                                               | otherwise = if checkSafeInsert board v i j
                                                                                then solveBoard (insertCellBoard board (Initial v r) i j []) (primeiro (drop (i+1) board)) (i+1) 0 (insertCellBoard board (Possible vs r) i j [])
                                                                                else solveBoard alternativeBoard ((Possible (vs) r):ys) i j alternativeBoard
                                                                where 
                                                                    primeiro xs | countLines xs == 0 = []
                                                                                | otherwise = head xs
solveBoard board ((Initial v r):xs) i j alternativeBoard | j < countLines board -1 = solveBoard board xs i (j+1) alternativeBoard
                                                         | otherwise = solveBoard board (primeiro (drop (i+1) board)) (i+1) 0 alternativeBoard
                                                         where 
                                                             primeiro xs | countLines xs == 0 = []
                                                                         | otherwise = head xs


{-- adiciona a celula na coluna i da linha Row --}
insertCellRow :: Row -> Cell -> Int -> Row -> Row
insertCellRow [] _ _ newRow = newRow
insertCellRow (x:xs) cell i newRow | i /= 0 = insertCellRow xs cell (i-1) (newRow ++ [x])
                                   | otherwise = insertCellRow xs cell (i-1) (newRow ++ [cell])

{-- adiciona a celula na linha x e coluna y do tabuleiro Board --}
insertCellBoard :: Board -> Cell -> Int -> Int -> Board -> Board
insertCellBoard [] _ _ _ newBoard = newBoard
insertCellBoard (r:rs) cell x y newBoard | x == 0 = insertCellBoard rs cell (x-1) y (newBoard ++ [insertCellRow r cell y []])
                                         | otherwise = insertCellBoard rs cell (x-1) y (newBoard ++ [r])