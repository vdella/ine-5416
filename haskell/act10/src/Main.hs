module Main where

-- Exercício 8
echoWithDo :: IO ()
echoWithDo = do
    x_as_char <- getChar
    putChar x_as_char

echoWithMonad :: IO ()
echoWithMonad = getChar >>= putChar

main :: IO ()
main = do
    -- echoWithDo
    echoWithMonad