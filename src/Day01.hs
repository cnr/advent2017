#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common
import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
    input <- map digitToInt . filter isDigit <$> readInput 1
    print $ firstProblem input
    print $ secondProblem input

firstProblem :: [Int] -> Int
firstProblem = captcha 1

secondProblem :: [Int] -> Int
secondProblem xs = captcha (length xs `div` 2) xs

captcha :: Int   -- Cycle offset for captcha
        -> [Int] -- Input data
        -> Int
captcha offset xs = sum [x | (x,y) <- captchaZip offset xs, x == y]

captchaZip :: Int -> [a] -> [(a,a)]
captchaZip offset xs = zip xs (drop offset (cycle xs))
