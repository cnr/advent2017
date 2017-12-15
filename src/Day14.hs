
module Main (main) where

import           Common
import           Data.Bits (testBit, popCount)
import qualified Data.Set as S
import           Data.Word
import           Knot

main :: IO ()
main = do
    input <- head . lines <$> readInput 14

    let rawResults = [hashS (input ++ "-" ++ show n) | n <- [0..127] :: [Int]]

    print (part1 rawResults)
    print (part2 rawResults)


part1 :: [[Int]] -> Int
part1 = sum . map popCount . concat

-- TODO: use fgl for this, just like day 12

part2 :: [[Int]] -> Int
part2 = length . go . resultsToGrid
    where
    go :: S.Set (Int,Int) -> [[(Int,Int)]]
    go squares
      | S.null squares = []
      | otherwise      = let seed  = S.findMin squares
                             group = dfs (neighbors squares) seed
                          in group : go (foldr S.delete squares group)

neighbors :: S.Set (Int,Int) -> (Int,Int) -> [(Int,Int)]
neighbors grid (x,y) = [pair | pair <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)], S.member pair grid]

resultsToGrid :: [[Int]] -> S.Set (Int,Int)
resultsToGrid grid = S.fromList [(x,y) | (y,xs) <- zip [0..] grid, (x,bit) <- zip [0..] (toBits . fromIntegral =<< xs), bit]

toBits :: Word8 -> [Bool]
toBits w = map (testBit w) [7,6..0]
