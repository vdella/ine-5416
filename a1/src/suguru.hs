import qualified Data.Char {-- Podemos usar isso? --}

data Cell =  Initial Int Int | Possible [Int] Int deriving (Show, Eq) {-- Uma celula pode ja estar definida ou ser um cojunto de possiveis valores --}
type Row = [Cell]
type Board = [Row]


readBoard :: String -> Row -> Board -> Board {-- Itera sobre a string (arquivo lido) e adiciona cada coluna ao tabuleiro (Board) --}
readBoard [] list board = board ++ [list] {-- Fim do arquivo, adiciona o a ultima linha lida --}
readBoard (x:xs) list board {-- Nao temos mais o que ler, adiciona linha ao tabuleiro --}
  | x == '\n' = readBoard xs [] (board ++ [list]) {-- \n Marca que terminamos de ler uma linha, entao adicionamos ao tabuleiro, lemos a proxima --}
  | x == ' ' = readBoard xs list board {-- Ignora espacos --}
  | x == '0' = readBoard xs (list ++ [Possible [1..9] 1]) board {-- Marcacao de valor a ser completado, preenchemos com lista de possiveis valores --}
  | otherwise = readBoard xs (list ++ [Initial (Data.Char.digitToInt x) 1]) board {-- valor previamente fixo --}
  
{-- dado o tabuleiro, indice e max de linhas, devolve as linhas adjacentes  a linha indice --}
adjecentRows :: Board -> Int -> Int -> [Row]
adjecentRows board i max | i == 0 = [board!!1]
                         | i == max = [board!!(i-2)]
                         | i >= max = []
                         | otherwise = [board!!(i-2) ++ board!!i]

{-- dado uma linha adjacente e o indice da celula original e o max de linhas, retorna as celulas adjacentes a essa celula (diagonais inclusas)--}
adjacentCells :: Row -> Int -> Int -> [Cell]
adjacentCells row i max | i == 0 = [row!!i] ++ [row!!(i+1)]
                        | i == max = [row!!(i-1)] ++ [row!!i]
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

{-- dado uma regiao, o tabuleiro e uma lista para armazenar os valores, retorna os valores possiveis desta regiao --}
getRegionPossibles :: Int -> Board -> [Cell] -> [Cell]
getRegionPossibles _ []  possibles = possibles
getRegionPossibles r (x:xs) possibles = getRowPossibles x possibles r ++ getRegionPossibles r xs possibles 


{-- 
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
--}

{-- lista [Int] contem Int --}
contains :: [Int] -> Int -> Bool
contains [] _ = False
contains (x:xs) i | x == i = True 
                  | otherwise = contains xs i


main :: IO ()
main = do
  s <- readFile "sudoku.txt"
  let board = readBoard s [] []
  print . show $ board
  print (show (adjecentRows board 2 3))
  putStrLn "Initials"
  print (show (getRegionInitials 1 board []))
  putStrLn "Possibles reg 1"
  print (show (getRegionPossibles 1 board []))
