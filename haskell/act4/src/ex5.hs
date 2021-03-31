avg x y z = (x * y * z) / 3

checkStudentGrade x y z = grade >= 6 where grade = avg x y z

main = do
    x_str <- getLine 
    y_str <- getLine 
    z_str <- getLine 

    let x = (read x_str :: Float)
    let y = (read y_str :: Float)
    let z = (read z_str :: Float)

    let result = avg x y z

    print result