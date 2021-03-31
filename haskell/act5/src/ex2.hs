avg :: [Int] -> Float 
avg [] = 0
avg values = result
               where sum' (a:b) = a + sum b
                     sum' [] = 0
                     result = fromIntegral (sum' values) / fromIntegral (length values)
                    
main = do
    let values = [2, 2, 2, 2, 2, 4]
    print (avg values)