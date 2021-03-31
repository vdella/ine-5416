canTriangleBeMadeFrom :: Float -> Float -> Float -> Bool 
canTriangleBeMadeFrom x y z = x + y >= z 
    

main = do
    char_x <- getLine 
    char_y <- getLine 
    char_z <- getLine 

    let x = (read char_x :: Float)
    let y = (read char_y :: Float)
    let z = (read char_z :: Float)

    let result1 = canTriangleBeMadeFrom x y z
    let result2 = canTriangleBeMadeFrom y z x
    let result3 = canTriangleBeMadeFrom z x y

    print (result1 && result2 && result3)