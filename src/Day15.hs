
module Main (main) where

import Data.Word

main :: IO ()
main = do
    print part1
    print part2

part1 :: Int
part1 = countMatches 40000000 gen1 gen2
    where
    gen1 = firstGen (const True)
    gen2 = secondGen (const True)

part2 :: Int
part2 = countMatches 5000000 gen1 gen2
    where
    gen1 = firstGen (\n -> n `mod` 4 == 0)
    gen2 = secondGen (\n -> n `mod` 8 == 0)

inputA, inputB :: Int
inputA = 618
inputB = 814

firstGen :: (Int -> Bool) -> [Word16]
firstGen filt = gen 16807 filt inputA

secondGen :: (Int -> Bool) -> [Word16]
secondGen filt = gen 48271 filt inputB

gen :: Int -- multiplier
    -> (Int -> Bool) -- filter
    -> Int -- seed
    -> [Word16]
gen mult filt = map lower16 . filter filt . iterate (next mult)

countMatches :: Eq a
             => Int -- number of elements to check
             -> [a] -> [a] -> Int
countMatches amount xs ys = length [() | match <- take amount (zipWith (==) xs ys), match]

next :: Int -> Int -> Int
next mult n = n * mult `mod` 2147483647

lower16 :: Int -> Word16
lower16 = fromIntegral
