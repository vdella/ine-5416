gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)

main = do
    char_x <- getLine
    char_y <- getLine

    let x = (read char_x :: Int)
    let y = (read char_y :: Int)

    print (gcd' x y)