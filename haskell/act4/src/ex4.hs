xor arg1 arg2 = arg1 /= arg2

main = do
    x_str <- getLine 
    y_str <- getLine 

    let x = (read x_str :: Bool)
    let y = (read y_str :: Bool)

    let result = xor x y

    print result