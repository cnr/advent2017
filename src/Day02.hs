#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common

main :: IO ()
main = do
    input <- map (map read . words) . lines <$> readInput 2
    print (checksum1 input)
    print (checksum2 input)

checksum1 :: [[Int]] -> Int
checksum1 = sum . map difference
    where
    difference xs = maximum xs - minimum xs

-- point-free for fun
-- checksum1' :: [[Int]] -> Int
-- checksum1' = sum . map ((-) <$> maximum <*> minimum)

checksum2 :: [[Int]] -> Int
checksum2 = sum . map divisible
    where
    divisible xs = head [x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0]
