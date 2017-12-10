#!/usr/bin/env stack
-- stack runghc

import Control.Monad.Writer
import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (chunksOf, splitOn)
import Text.Printf (printf)

main :: IO ()
main = do
    rawInput <- head . lines <$> readFile "input.txt"

    let lengths1 = map read (splitOn "," rawInput)
        lengths2 = map ord rawInput ++ [17, 31, 73, 47, 23] -- part 2 extra lengths

    print $ part1 lengths1
    print $ part2 lengths2

part1 :: [Int] -> Int
part1 = product . take 2 . runRound

part2 :: [Int] -> String
part2 = concatMap (showHexByte . foldr xor 0) . chunksOf 16 . runRound . concat . replicate 64

runRound :: [Int] -> [Int]
runRound xs = runRotation (foldl1 (>=>) (zipWith step [0..] xs) [0..255])

step :: Int -> Int -> [a] -> Rotation a
step skip len xs = rotateR (len + skip) (reverse (take len xs) ++ drop len xs)

-- The 'cute' solution

type Rotation a = Writer (Sum Int) [a]

runRotation :: Rotation a -> [a]
runRotation = uncurry fixRotation . runWriter
    where
    fixRotation xs (Sum rot) = rotate (length xs - (rot `mod` length xs)) xs

showHexByte :: Int -> String
showHexByte = printf "%02x"

rotateR :: Int -> [a] -> Rotation a
rotateR n xs = rotate n xs <$ tell (Sum n)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))
