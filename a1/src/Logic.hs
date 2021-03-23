module Logic where

import Types
import Debug.Trace

itopoint :: Int -> Int -> (Int, Int)
itopoint i max = (calcX i, calcY i)
    where calcY i = i - max * div i max
          calcX i = div i max
          
pointToi :: (Int, Int) -> Int -> Int
pointToi (i, j) max = j + i * max

adjRow :: Int -> Int -> Board -> [Cell]
adjRow i max board = helper (itopoint i max)
    where helper (i, j) | (j-1) < 0 = [board !! pointToi (i, j+1) max]
                        | (j+1) >= max = [board !! pointToi (i, j-1) max]
                        | otherwise = [board !! pointToi (i, j+1) max] ++ [board !! pointToi (i, j-1) max]

adjCol :: Int -> Int -> Board -> [Cell]
adjCol i max board = helper (itopoint i max)
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

cellsToVals :: [Cell] -> [Int]
cellsToVals [] = []
cellsToVals ((Initial v r):xs) = [v] ++ cellsToVals xs
                                      
cellRegion :: Cell -> Int
cellRegion (Initial _ r) = r

cellValue :: Cell -> Int
cellValue (Initial v _) = v

inRegion :: Int -> Int -> Board -> [Cell] 
inRegion _ _ [] = []
inRegion region max ((Initial v r):xs) | region == r = [Initial v r] ++ inRegion region max xs
                                       | otherwise = inRegion region max xs

regionLength :: Int -> Int -> Board -> Int
regionLength region max board = length (inRegion region max board)
                                       
removeVal :: [Int] -> [Int] -> [Int]
removeVal [] _ = []
removeVal xs [] = xs
removeVal xs (y:ys) = removeVal (removeAll y xs) ys

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll y (x:xs) | x == y = removeAll y xs
                   | otherwise = [x] ++ removeAll y xs
                   
possiblesAt :: Int -> Int -> Board -> [Int]
possiblesAt i max board | i > length board = []
                    | cellValue( (board !! i)) == 0 = [1..rmax] `removeVal` (cellsToVals ((adjRow i max board) ++ (adjCol i max board) ++ ( inRegion (cellRegion (board!!i)) max board)))
                    | otherwise = [cellValue (board !! i)]
                    where rmax = regionLength (cellRegion (board !! i)) max board
                    
tryInsert :: Int -> Board -> Cell -> Board
tryInsert i board cell = take i board ++ [cell] ++ drop (i + 1) board


nextBlank :: Int -> Board -> Int
nextBlank i board | i == length board -1 = length board -1
                  | cellValue (board !! (i + 1)) == 0 = i + 1
                  | otherwise = nextBlank (i + 1) board
                  
solve :: Int -> Int -> Board -> [Int] -> Board
solve 24 max board [] = []
solve 24 max board (x:[]) = tryInsert 24 board (Initial x (cellRegion (board!!24)))
solve 24 max board (x:_) = []
solve _ max board [] = []
solve i max board (x:xs) | solvedAhead == [] = solve i max board xs
                         | otherwise = solvedAhead
                         where solveNext i board = solve (nextBlank i board) max board (possiblesAt (nextBlank i board) max board)
                               solvedAhead = solveNext i (tryInsert i board (Initial x region))
                               region = cellRegion (board!!i)
                               
solveBoard :: Int -> Board -> Board
solveBoard max board = solve 0 max board (possiblesAt 0 max board)