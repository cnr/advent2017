#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Data.Maybe
import qualified Data.Map.Strict as M

input :: Int
input = 265149

main :: IO ()
main = do
    print part1
    print part2


part1 :: Int
part1 = distances !! input

part2 :: Int
part2 = head $ dropWhile (<= input) values


---- Calculating distances from the center for part 1

allSides :: Int -- layer, where 1 is the origin
         -> [Int]
allSides = concat . replicate 4 . oneSide

oneSide :: Int -> [Int]
oneSide n = take (n*2) [n + desc | desc <- abs <$> [(n-1),(n-2)..]]

distances :: [Int] -- (!! input)
distances = 0 : 0 : (allSides =<< [1..])


---- Calculating values & coordinates, with the center as the origin, for part 2

-- Way, way overcomplicated

type Grid = M.Map (Int,Int) Int

values :: [Int]
values = go (M.singleton (0,0) 1) coords
    where
    go :: Grid -> [(Int,Int)] -> [Int]
    go _    []     = error "impossible"
    go grid (x:xs) = let value = sum (neighborValues grid x)
                      in value : go (M.insert x value grid) xs

    neighborValues :: Grid -> (Int,Int) -> [Int]
    neighborValues grid point = mapMaybe (`M.lookup` grid) (neighbors point)

    neighbors :: (Int,Int) -> [(Int,Int)]
    neighbors (x,y) = [(x',y') | x' <- [x-1 .. x+1], y' <- [y-1 .. y+1]]

coords :: [(Int,Int)] -- starting at the origin
coords = (0,0) : (layerCoords =<< [1..])

layerCoords :: Int -- layer, where 1 is the first layer surrounding the origin
            -> [(Int,Int)]
layerCoords n = concat [f n | f <- [ascY, descX, descY, ascX]]

sideCoordDiff :: Int -> [Int]
sideCoordDiff n = take (n*2) [desc | desc <- negate <$> [(n-1),(n-2)..]]

ascY :: Int -> [(Int,Int)]
ascY n = [(n, side) | side <- sideCoordDiff n]

descY :: Int -> [(Int,Int)]
descY n = [(-n, side) | side <- negate <$> sideCoordDiff n]

ascX :: Int -> [(Int,Int)]
ascX n = [(side, -n) | side <- sideCoordDiff n]

descX :: Int -> [(Int,Int)]
descX n = [(side, n) | side <- negate <$> sideCoordDiff n]
