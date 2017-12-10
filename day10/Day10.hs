#!/usr/bin/env stack
-- stack runghc

import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (chunksOf, splitOn)
import Data.Monoid ((<>))
import Text.Printf (printf)

main :: IO ()
main = do
    rawInput <- head . lines <$> readFile "input.txt"

    let lengths1 = map read (splitOn "," rawInput)
        lengths2 = map ord rawInput ++ [17, 31, 73, 47, 23] -- part 2 extra lengths

    print $ part1 lengths1
    print $ part2 lengths2

part1 :: [Int] -> Int
part1 = combine . runRounds
    where
    combine   = product . take 2
    runRounds = runRotation [0..255] . mconcat . zipWith step [0..]

part2 :: [Int] -> String
part2 = combine . runRounds
    where
    combine   = concatMap (showHexByte . foldr xor 0) . chunksOf 16
    runRounds = runRotation [0..255] . mconcat . zipWith step [0..] . concat . replicate 64

step :: Int -> Int -> Rotation a
step skip len = liftR (\xs -> reverse (take len xs) ++ drop len xs) <> rotateR (len + skip)

-- The 'cute' solution

newtype Rotation a = Rotation { performRotation :: [a] -> (Int, [a]) }

runRotation :: [a] -> Rotation a -> [a]
runRotation xs = uncurry fixRotation . (`performRotation` xs)
    where
    fixRotation rot = rotate (length xs - (rot `mod` length xs))

instance Monoid (Rotation a) where
    mempty = Rotation (\xs -> (0, xs))
    mappend (Rotation first) (Rotation second) = Rotation $ \xs -> let (rot1, xs')  = first xs
                                                                       (rot2, xs'') = second xs'
                                                                    in (rot1 + rot2, xs'')

showHexByte :: Int -> String
showHexByte = printf "%02x"

rotateR :: Int -> Rotation a
rotateR n = Rotation (\xs -> (n, rotate n xs))

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

liftR :: ([a] -> [a]) -> Rotation a
liftR f = Rotation (\xs -> (0, f xs))
