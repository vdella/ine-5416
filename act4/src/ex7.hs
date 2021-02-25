fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

main = do
    char_n <- getLine 
    let n = (read char_n :: Int)
    print (fib n)