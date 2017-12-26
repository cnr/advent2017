#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import           Common
import qualified Data.Map.Strict as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Data.Graph.Inductive.Graph (mkUGraph)
import           Data.Graph.Inductive.PatriciaTree (UGr)
import           Data.Graph.Inductive.Query.DFS (components)

main :: IO ()
main = do
    input <- M.fromList <$> readParsedLines pipesP 12
    print (part1 input)
    print (part2 input)

part1 :: M.Map Int [Int] -> Int
part1 pipes = length (dfs (pipes M.!) 0)

part2 :: M.Map Int [Int] -> Int
part2 pipes = length . components $ (mkUGraph (M.keys pipes) (sequence =<< M.toList pipes) :: UGr)

pipesP :: Parser (Int, [Int])
pipesP = (,) <$> decimal <* string " <-> " <*> (decimal `sepBy` string ", ")
