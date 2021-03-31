count :: [Int] -> Int -> Int 
count [] a = 0
count [a] b = 0
count (a:b) value =
        if value == a then
            1 + count (tail (a:b)) value
        else
            count (tail (a:b)) value
                        

main = do
    let values = [9, 10005, 9, 7, 2, 8]
    print (count values 7)
    print values