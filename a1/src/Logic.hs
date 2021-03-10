module Logic where

import Types

{-- dado o tabuleiro, indice e max de linhas, devolve as linhas adjacentes  a linha indice --}
adjecentRows :: Board -> Int -> Int -> [Row]
adjecentRows board i max | i == 0 = [board!!1]
                         | i == max = [board!!(i-1)]  
                         | i >= max = []
                         | otherwise = [board!!(i-1) ++ board!!i]


{-- dado uma linha adjacente e o indice da celula original e o max de linhas, retorna as celulas adjacentes a essa celula (diagonais inclusas)--}
adjacentCells :: Row -> Int -> Int -> [Cell]
adjacentCells row i max | i == 0 = (row!!i) : [row!!(i+1)]
                        | i == max = (row!!(i-1)) : [row!!i]
                        | otherwise = [row!!(i-1)] ++ [row!!i] ++ [row!!(i+1)]


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

{-- dada uma linha, emove uma lista de valores dos valores possiveis de uma regiao --}
{-- Valores a serem tirados, regiao, linha, vazio--}
trimRegion :: [Value] -> Region -> Row -> Row -> Row
trimRegion _ _ [] newRow = newRow
trimRegion vals reg ((Possible ys r):xs) newRow | reg == r = trimRegion vals reg xs (newRow ++ [Possible (removeValues vals ys) r]) -- se for a regiao alvo, remove o valor da lista de Possible
                                                | otherwise = trimRegion vals reg xs (newRow ++ [Possible ys r]) -- caso contrario nao mudamos nasa
                                               where
                                                   removeValues [] ys = ys
                                                   removeValues (x:xs) ys = removeValues xs (removeItem x ys)
trimRegion val reg ((Initial v r):xs) newRow = trimRegion val reg xs (newRow ++ [Initial v r]) -- se for um valor ja certo, nao fazemos modificacao

{-- Dado uma lista de valores, uma regiao e o tabuleiro, retira todos os valores contido na lista dos valores possiveis desta regiao de todo o tabuleiro--}
trimBoard :: [Value] -> Region -> Board -> Board -> Board
trimBoard  _ _ [] newBoard = newBoard
trimBoard vals reg (x:xs) newBoard = trimBoard vals reg xs (newBoard ++ [trimRegion vals reg x []])

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