
module Knot
    ( hashS
    , hash
    , showHash

    , runRound
    ) where

import Control.Monad.Writer
import Data.Bits       (xor)
import Data.Char       (ord)
import Data.List.Split (chunksOf)
import Text.Printf     (printf)

hashS :: String -> [Int]
hashS = hash . map ord

hash :: [Int] -> [Int]
hash = map (foldr xor 0) . chunksOf 16 . runRound . concat . replicate 64 . (++ [17, 31, 73, 47, 23])

showHash :: [Int] -> String
showHash = concatMap (printf "%02x")


type Rotation a = Writer (Sum Int) [a]

runRound :: [Int] -> [Int]
runRound xs = runRotation (foldl1 (>=>) (zipWith step [0..] xs) [0..255])

step :: Int -> Int -> [a] -> Rotation a
step skip len xs = rotateR (len + skip) (reverse (take len xs) ++ drop len xs)

runRotation :: Rotation a -> [a]
runRotation = uncurry fixRotation . runWriter
    where
    fixRotation xs (Sum rot) = rotate (length xs - (rot `mod` length xs)) xs

rotateR :: Int -> [a] -> Rotation a
rotateR n xs = rotate n xs <$ tell (Sum n)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))
