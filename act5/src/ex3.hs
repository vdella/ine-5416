min' :: [Int] -> Int
min' [] = 0
min' [v] = v
min' (a:b) = if a < head rest
             then min' (a:tail rest)
             else min' rest
             where rest = tail (a:b)


main = do
    let values = [9, 100, 50, 7, -1, 8]
    print (min' values)