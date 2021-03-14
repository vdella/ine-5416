module Grid where

import Types (Board, Point)

board :: [[Int]]
board = [[0, 0, 0, 0, 5],
         [0, 0, 0, 0, 0],
         [0, 4, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 2, 3, 0, 0]]

adjacentValuesAt :: Point -> [Int]
adjacentValuesAt coordinates
    | i == 0 && j == 0 = [(board!!i)!!(j + 1), (board!!(i + 1))!!(j + 1), (board!!(i + 1))!!j] 
    | i == 0 && j == upperBound = [(board!!i)!!(j - 1), (board!!(i + 1))!!j, (board!!(i + 1))!!(j - 1)]
    | i == upperBound && j == 0 = [(board!!i)!!(j + 1), (board!!(i - 1))!!j, (board!!(i - 1))!!(j + 1)]
    | i == upperBound && j == upperBound = [(board!!(i - 1))!!j, (board!!i)!!(j - 1), (board!!(i - 1))!!(j - 1)]
    | otherwise = [(board!!(i - 1))!!(j - 1), (board!!(i - 1))!!j, (board!!(i - 1))!!(j + 1),
                   (board!!i)!!(j - 1), (board!!i)!!(j + 1),
                   (board!!(i + 1))!!(j - 1), (board!!(i + 1))!!j, (board!!(i + 1))!!(j + 1)]
    where upperBound = length (head board) - 1
          i = fst coordinates
          j = snd coordinates

region :: Int -> [Point]
region 1 = [(0, 0), (0, 1), (1, 1), (1, 2), (2, 2)]
region 2 = [(1, 0), (2, 0), (2, 1), (3, 0), (3, 1)]
region 3 = [(4, 0), (4, 1), (4, 2), (3, 2)]
region 4 = [(2, 3), (2, 4)]
region 5 = [(0, 2), (0, 3), (0, 4), (1, 3), (1, 4)]
region 6 = [(3, 3), (3, 4), (4, 3), (4, 4)]

initialValuesForRegion :: Int -> [Int]
initialValuesForRegion 1 = []
initialValuesForRegion 2 = [4]
initialValuesForRegion 3 = [2, 3]
initialValuesForRegion 4 = []
initialValuesForRegion 5 = [5]
initialValuesForRegion 6 = []

possibleValuesForRegion :: Int -> [Int]
possibleValuesForRegion 1 = [1, 2, 3, 4, 5]
possibleValuesForRegion 2 = [1, 2, 3, 5]
possibleValuesForRegion 3 = [1, 4]
possibleValuesForRegion 4 = [1, 2]
possibleValuesForRegion 5 = [1, 2, 3, 4]
possibleValuesForRegion 6 = [1, 2, 3, 4]
