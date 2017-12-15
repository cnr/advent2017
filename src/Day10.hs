#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common
import Data.List.Split (splitOn)
import Knot

main :: IO ()
main = do
    rawInput <- head . lines <$> readInput 10

    print $ part1 (map read (splitOn "," rawInput))
    print $ part2 rawInput

part1 :: [Int] -> Int
part1 = product . take 2 . runRound

part2 :: String -> String
part2 = showHash . hashS
