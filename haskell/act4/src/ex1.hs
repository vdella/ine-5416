import Data.Char(digitToInt)

power :: Int -> Int-> Int
power x 0 = 1
power x y = x * power x (y - 1)

main = do
       char_as_x <- getLine
       char_as_y <- getLine

       let x = (read char_as_x :: Int)
       let y = (read char_as_y :: Int)

       let result = power x y

       print result