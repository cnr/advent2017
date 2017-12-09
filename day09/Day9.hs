#!/usr/bin/env stack
-- stack runghc

import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = do
    input <- head . lines <$> readFile "input.txt"
    print (unsafeParse (groupP 1) input)

groupP :: Int -> Parser (Int,Int)
groupP depth = do
    groups <- between (char '{') (char '}') ((groupP (depth + 1) <|> garbageP) `sepBy` char ',')
    return (depth + sum (map fst groups), sum (map snd groups))

garbageP :: Parser (Int,Int)
garbageP = do
    values <- between (char '<') (char '>') (many (0 <$ (char '!' *> anyChar) <|> 1 <$ (satisfy (/= '>'))))
    return (0, sum values)

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = result where Right result = parse parser "input" input
