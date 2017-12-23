
module Main (main) where

import Common
import Data.Array
import Data.Char (isLetter)

main :: IO ()
main = do
    input <- readInput 19

    let tiles = [((x,y), fromChar c) | (y,line) <- zip [0..] (lines input), (x,c) <- zip [0..] line]
        maxIx = maximum (map fst tiles)

        grid  = array ((0,0),maxIx) tiles
        start = head [coord | (coord@(_,0), Vert) <- tiles]

    print (part1 grid start)
    print (part2 grid start)

part1 :: Grid -> (Int,Int) -> [Char]
part1 grid start = [c | Letter c <- walk grid South start]

part2 :: Grid -> (Int,Int) -> Int
part2 grid start = length (walk grid South start) - 1 -- -1 for the end tile

walk :: Grid -> Direction -> (Int,Int) -> [Tile]
walk grid = go
    where
    go :: Direction -> (Int,Int) -> [Tile]
    go direction coord = grid ! coord :
        case grid ! coord of
            Junction -> let dir = head [ newDirection
                                       | newDirection <- [North,South,East,West]
                                       , let tile = grid ! move newDirection coord
                                       , tile == perpendicular direction
                                       ]
                         in go dir (move dir coord)
            Horiz -> go direction (move direction coord)
            Vert  -> go direction (move direction coord)
            Letter _ -> go direction (move direction coord)
            End  -> []

move :: Direction -> (Int,Int) -> (Int,Int)
move North (x,y) = (x,y-1)
move South (x,y) = (x,y+1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)

perpendicular :: Direction -> Tile
perpendicular North = Horiz
perpendicular South = Horiz
perpendicular East  = Vert
perpendicular West  = Vert

type Grid = Array (Int,Int) Tile

data Tile = Junction -- +
          | Horiz    -- -
          | Vert     -- |
          | Letter Char
          | End
          deriving (Eq, Show)

data Direction = North | East | South | West deriving Show

fromChar :: Char -> Tile
fromChar '+' = Junction
fromChar '-' = Horiz
fromChar '|' = Vert
fromChar c
  | isLetter c = Letter c
  | otherwise  = End

