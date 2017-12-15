#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import           Common
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

main :: IO ()
main = do
    banks <- V.fromList . map read . words <$> readInput 6
    print (solve banks)

solve :: V.Vector Int -> (Int, Int) -- (part 1, part 2)
solve = go M.empty . zip [0..] . iterate step
    where

    go _    []            = error "impossible"
    go seen ((cur,x):xs) =
        case M.lookup x seen of
            Just prev -> (cur, cur - prev)
            Nothing   -> go (M.insert x cur seen) xs

step :: V.Vector Int -> V.Vector Int
step banks = let largest = maximum banks
                 count   = V.length banks
                 Just ix = V.elemIndex largest banks

                 additions = [(x `mod` count, 1) | x <- [ix+1..ix+largest]]
              in V.accum (+) (banks V.// [(ix,0)]) additions
