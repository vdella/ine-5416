-- TODO: check NaN's at the end of execution.

bhaskara :: Float -> Float -> Float -> (Float, Float)
bhaskara a b c = ((-b + sqrt delta) / 2*a, (-b - sqrt delta) / 2*a)
                where delta = b^2 - 4*a*c

main = do
    char_x <- getLine
    char_y <- getLine
    char_z <- getLine

    let x = (read char_x :: Float)
    let y = (read char_y :: Float)
    let z = (read char_z :: Float)

    print (bhaskara x y z)