getBiggestFrom :: [Float] -> Float
getBiggestFrom list = 
    let begin = getFrom 0 in
    let mid = getFrom 1 in
    let end = getFrom 2
    in 
        if begin >= mid && begin >= end then begin
        else 
            if mid >= begin && mid >= end then mid 
            else end
    where getFrom index = list !! index


main = do
    char_x <- getLine 
    char_y <- getLine 
    char_z <- getLine 

    let x = (read char_x :: Float)
    let y = (read char_y :: Float)
    let z = (read char_z :: Float)

    let list = [x, y, z]

    print (getBiggestFrom list)