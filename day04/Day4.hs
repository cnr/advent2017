#!/usr/bin/env stack
-- stack runghc

import Data.List

main :: IO ()
main = do
    print . length . filter (\xs -> nub xs == xs) . map words . lines =<< readFile "input.txt" -- part 1
    print . length . filter (\xs -> nub xs == xs) . map (map sort . words) . lines =<< readFile "input.txt" -- part 2
