min' :: [Int] -> Int
min' [] = 0
min' [v] = v
min' (a:b) = if a < head rest
             then min' (a:tail rest)
             else min' rest
             where rest = tail (a:b)

max' :: [Int] -> Int
max' [] = 0
max' [v] = v
max' (a:b) = if a > head rest
             then max' (a:tail rest)
             else max' rest
             where rest = tail (a:b)


diff :: [Int] -> Int
diff [] = 0
diff [v] = v
diff values = max' values - min' values

main = do
    let values = [9, 10005, 50, 7, 2, 8]
    print (diff values)
    print values