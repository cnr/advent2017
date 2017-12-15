#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = print =<< readParsed (groupP 1) 9

groupP :: Int -> Parser (Int,Int)
groupP depth = do
    groups <- between (char '{') (char '}') ((groupP (depth + 1) <|> garbageP) `sepBy` char ',')
    return (depth + sum (map fst groups), sum (map snd groups))

garbageP :: Parser (Int,Int)
garbageP = do
    values <- between (char '<') (char '>') (many (0 <$ (char '!' *> anyChar) <|> 1 <$ (satisfy (/= '>'))))
    return (0, sum values)
