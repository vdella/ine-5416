import qualified Data.Char {-- Podemos usar isso? --}

data Cell =  Initial Int | Possible [Int] deriving (Show, Eq) {-- Uma celula pode ja estar definida ou ser um cojunto de possiveis valores --}
type Row = [Cell]
type Board = [Row]

readBoard :: String -> Row -> Board -> Board {-- Itera sobre a string (arquivo lido) e adiciona cada coluna ao tabuleiro (Board) --}
readBoard [] list board = board ++ [list] {-- Fim do arquivo, adiciona o a ultima linha lida --}
readBoard (x:xs) list board {-- Nao temos mais o que ler, adiciona linha ao tabuleiro --}
  | x == '\n' = readBoard xs [] (board ++ [list]) {-- \n Marca que terminamos de ler uma linha, entao adicionamos ao tabuleiro, lemos a proxima --}
  | x == ' ' = readBoard xs list board {-- Ignora espacos --}
  | x == '0' = readBoard xs (list ++ [Possible [1..9]]) board {-- Marcacao de valor a ser completado, preenchemos com lista de possiveis valores --}
  | otherwise = readBoard xs (list ++ [Initial (Data.Char.digitToInt x)]) board {-- valor previamente fixo --}
  
{-- dado o tabuleiro, indice e max de linhas, devolve as linhas adjacentes --}
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
