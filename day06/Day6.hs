#!/usr/bin/env stack
-- stack runghc

import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

main :: IO ()
main = do
    banks <- V.fromList . map read . words <$> readFile "input.txt"
    print (solve banks)

solve :: V.Vector Int -> (Int, Int) -- (part 1, part 2)
solve = go M.empty . zip [0..] . iterate step
    where

    go seen ((step,x):xs) =
        case M.lookup x seen of
            Just prev -> (step, step - prev)
            Nothing   -> go (M.insert x step seen) xs

step :: V.Vector Int -> V.Vector Int
step banks = let max       = maximum banks
                 count     = V.length banks
                 Just ix   = V.elemIndex max banks

                 additions = [(x `mod` count, 1) | x <- [ix+1..ix+max]]
              in V.accum (+) (banks V.// [(ix,0)]) additions
