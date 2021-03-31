getDistanceOf :: [Float] -> [Float] -> Float
getDistanceOf src dst = sqrt (x_op + y_op + z_op) 
                        where x_op = moduleOf 0
                              y_op = moduleOf 1
                              z_op = moduleOf 2
                              moduleOf pos = (dst !! pos - src !! pos)^2

main = do
    char_x1 <- getLine
    char_y1 <- getLine
    char_z1 <- getLine

    char_x2 <- getLine
    char_y2 <- getLine
    char_z2 <- getLine

    let x1 = (read char_x1 :: Float)
    let y1 = (read char_y1 :: Float)
    let z1 = (read char_z1 :: Float)

    let pointA = [x1, y1, z1]

    let x2 = (read char_x2 :: Float)
    let y2 = (read char_y2 :: Float)
    let z2 = (read char_z2 :: Float)

    let pointB = [x2, y2, z2]

    print (getDistanceOf pointA pointB)