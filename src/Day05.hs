#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common
import Data.Vector (Vector, (!?), (//), fromList)

main :: IO ()
main = do
    input <- fromList . map read . lines <$> readInput 5
    print $ solve (+1) input -- part 1
    print $ solve (\x -> if x < 3 then x + 1 else x - 1) input -- part 2

-- should probably make this mutable to speed it up
solve :: (Int -> Int) -> Vector Int -> Int
solve f = go 0 0

    where

    go :: Int -> Int -> Vector Int -> Int
    go steps posn jumps =
        case jumps !? posn of
          Just jmp -> go (steps + 1) (posn + jmp) $! (jumps // [(posn, f jmp)])
          Nothing  -> steps
