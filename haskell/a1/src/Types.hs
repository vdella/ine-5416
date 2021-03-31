module Types where

type Region = Int
type Value = Int

data Cell = Initial Value Region deriving (Show, Eq)

type Board = [Cell]