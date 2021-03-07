module Types where

type Region = Int
type Value = Int

data Cell = Initial Value Region | Possible [Value] Region deriving (Show, Eq)
type Row = [Cell]
type Board = [Row]

type RegionCoordinates = ()
type ParsedFile = (Board, RegionCoordinates)