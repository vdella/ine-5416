module Logic where

import Types

{-- dado o tabuleiro, indice e max de linhas, devolve as linhas adjacentes  a linha indice --}
adjecentRows :: Board -> Int -> Int -> [Row]
adjecentRows board i max | i == 0 = [board!!1]
                         | i == max = [board!!(i-2)]
                         | i >= max = []
                         | otherwise = [board!!(i-2) ++ board!!i]


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


{-- dado um valor e uma regiao, remove o valor das celulas do tipo Possible --}

{-- 
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
--}


contains :: [Int] -> Int -> Bool
contains [] _ = False
contains (x:xs) i | x == i = True
                  | otherwise = contains xs i