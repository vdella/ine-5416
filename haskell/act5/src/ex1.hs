reduce :: [Int] -> Int
reduce [] = 0
reduce (begin:end) = begin + reduce end

main = do
    let list = [1, 2, 3, 4, 5, 6, 7, 8, 9] -- Sum must be 45.
    print (reduce list)