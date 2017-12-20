
-- TODO: speed this up

module Main (main) where

import Data.List

main :: IO ()
main = do
    print part1
    print part2

input :: Int
input = 363

part1 :: [Int]
part1 = take 2 $ foldl' step [0,1] [2..2017]

part2 :: [Int]
part2 = take 2 . dropWhile (/= 0) $ foldl' step [0,1] [2..50000000]

step :: [Int] -> Int -> [Int]
step xs n = insertR n (rot input xs)

rot :: Int -> [a] -> [a]
rot n xs = take (length xs) (drop n (cycle xs))

insertR :: a -> [a] -> [a]
insertR x = (x:) . rot 1
