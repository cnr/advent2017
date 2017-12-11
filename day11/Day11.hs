#!/usr/bin/env stack
-- stack runghc

import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = do
    directions <- unsafeParse (directionP `sepBy` char ',') <$> readFile "input.txt"
    let results = scanl1 add directions

    print (distance (last results))
    print (maximum (map distance results))

type Direction = (Int, Int, Int)

add :: Direction -> Direction -> Direction
add (q,r,s) (q',r',s') = (q + q', r + r', s + s')

distance :: Direction -> Int
distance (q,r,s) = maximum [abs q, abs r, abs s]

n, ne, nw, s, se, sw :: Direction
n  = ( 0,-1, 1)
ne = ( 1,-1, 0)
nw = (-1, 0, 1)
s  = ( 0, 1,-1)
se = ( 1, 0,-1)
sw = (-1, 1, 0)

directionP :: Parser Direction
directionP = ne <$ string "ne"
         <|> nw <$ string "nw"
         <|> se <$ string "se"
         <|> sw <$ string "sw"
         <|> n  <$ string "n"
         <|> s  <$ string "s"

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = result where Right result = parse parser "input" input
