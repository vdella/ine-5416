import qualified Data.Char {-- Podemos usar isso? --}

data Cell =  Initial Int | Possible [Int] deriving (Show, Eq) {-- Uma celula pode ja estar definida ou ser um cojunto de possiveis valores --}
type Region = [Cell]
type Board = [Region]

readBoard :: String -> Region -> Board -> Board {-- Itera sobre a string (arquivo lido) e adiciona cada coluna ao tabuleiro (Board) --}
readBoard [] list board = board ++ [list] {-- Fim do arquivo, adiciona o a ultima linha lida --}
readBoard (x:xs) list board {-- Nao temos mais o que ler, adiciona linha ao tabuleiro --}
  | x == '\n' = readBoard xs [] (board ++ [list]) {-- \n Marca que terminamos de ler uma linha, entao adicionamos ao tabuleiro, lemos a proxima --}
  | x == ' ' = readBoard xs list board {-- Ignora espacos --}
  | x == '0' = readBoard xs (list ++ [Possible [1..9]]) board {-- Marcacao de valor a ser completado, preenchemos com lista de possiveis valores --}
  | otherwise = readBoard xs (list ++ [Initial (Data.Char.digitToInt x)]) board {-- valor previamente fixo --}
  

main :: IO ()
main = do
  s <- readFile "sudoku.txt"
  let lista = readBoard s [] []
  print . show $ lista
