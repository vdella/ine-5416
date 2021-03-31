class (Integral x, Ord x) => MeuInt x where

bigger :: Ord x => x -> x -> x
bigger a b
    | a > b = a
    | otherwise = b

smaller :: Ord x => x -> x -> x
smaller a b
    | a == bigger a b = b
    | otherwise = a

par :: Integral x => x -> Bool
par = even

impar :: Integral x => x -> Bool
impar = odd

{-|
    Not only checks if a given number is bigger than 1,
    but also does a list comprehension check: the null
    function returns True if a given list is totally empty.
    In other words, if x is, somewhat, divisible by y, then,
    null will give us False.

    Note: since sqrt x * sqrt x = x, in order to know if
          a number is prime we do not need to iterate
          until we get to x - 1; it's advised to iterate
          only until sqrt x, such as if a number is not divisible
          by any other until it reaches it's square root
|-}
primo :: (Integral x, Floating x) => x -> Bool
primo x = (x > 1) && null [y | y <- [2..sqrt x], mod x y == 0]

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)


instance MeuInt Integer
instance MeuInt Int

main = do
print (bigger (4::Integer) (12::Integer))
print (smaller (4::Integer) (12::Integer))