main = do
    base_str <- getLine
    height_str <- getLine 

    let base = (read base_str :: Int)
    let height = (read height_str :: Int)

    let result = div (base*height) 2

    print result