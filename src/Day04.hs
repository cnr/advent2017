#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common
import Data.List

main :: IO ()
main = do
    print . length . filter (\xs -> nub xs == xs) . map words . lines =<< readInput 4
    print . length . filter (\xs -> nub xs == xs) . map (map sort . words) . lines =<< readInput 4
