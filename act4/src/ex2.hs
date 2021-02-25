absolute = abs

main = do
    char_as_x <- getLine 

    let x = (read char_as_x :: Int)
    let result = absolute x
    
    print result