data Form = Circle Float | Rectangle Float Float | Triangle Float Float

area :: Form -> Float
area (Circle r) = pi * r * r
area (Rectangle b h) = b * h
area (Triangle b h) = (b * h) / 2

main = do
    print (area (Triangle 2 2))