min' :: [Int] -> Int
min' [] = 0
min' [v] = v
min' values = if first < second then do
                  let others = drop 1 (tail values) in
                      min' (first:others)
              else min' (tail values)
              where first = head values
                    second = values !! 1


main = do
    let values = [9, 100, 50, 7, -1, 8]
    print (min' values)