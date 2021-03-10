xor :: Bool -> Bool -> Bool
xor = \x y -> x /= y

main = do
    x_as_char <- getLine 
    y_as_char <- getLine 

    let x = (read x_as_char :: Bool)
    let y = (read y_as_char :: Bool)

    print (xor x y)
