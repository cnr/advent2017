#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = do
    directions <- readParsed (directionP `sepBy` char ',') 11
    let results = scanl1 add directions

    print (distance (last results))
    print (maximum (map distance results))

type Direction = (Int, Int, Int)

add :: Direction -> Direction -> Direction
add (q,r,s) (q',r',s') = (q + q', r + r', s + s')

distance :: Direction -> Int
distance (q,r,s) = maximum [abs q, abs r, abs s]

directionP :: Parser Direction
directionP = ( 1,-1, 0) <$ string "ne"
         <|> (-1, 0, 1) <$ string "nw"
         <|> ( 1, 0,-1) <$ string "se"
         <|> (-1, 1, 0) <$ string "sw"
         <|> ( 0,-1, 1) <$ string "n"
         <|> ( 0, 1,-1) <$ string "s"
