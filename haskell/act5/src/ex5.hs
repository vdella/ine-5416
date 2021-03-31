search :: [Int] -> Int -> Bool
search [] v = False
search (a:b) value = begin == value || search (tail (a:b)) value
                     where begin = head (a:b)

main = do
    let values = [9, 10005, 50, 7, 2, 8]
    print (search values 3)
    print values