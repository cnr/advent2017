
module Main (main) where

import Common
import Control.Monad   (replicateM)
import Data.List       (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
    rules <- readParsed rulesP 21
    
    print (countAfter 5 rules)
    print (countAfter 18 rules)

countAfter :: Int -> Rules -> Int
countAfter n rules = length . filter (== '#') . concat $ iterate (step rules) initial !! n

step :: Rules -> [String] -> [String]
step rules xs
  | even (length xs) = withBroken 2 3 (rules M.!) xs
  | otherwise        = withBroken 3 4 (rules M.!) xs

withBroken :: Int -> Int -> ([[a]] -> [[a]]) -> [[a]] -> [[a]]
withBroken fromSize toSize f = assembleGrid toSize . map (map f) . breakGrid fromSize

breakGrid :: Int -> [[a]] -> [[[[a]]]]
breakGrid n = transpose . map (chunksOf n) . transpose . map (chunksOf n)

assembleGrid :: Int -> [[[[a]]]] -> [[a]]
assembleGrid n = concatMap (map concat . transpose . map (chunksOf n . concat))

initial :: [String]
initial = [".#.","..#","###"]

type Rules  = M.Map Square Square
type Rule   = (Square,Square)
type Square = [String]

rulesP :: Parser Rules
rulesP = mkRules <$> ruleP `sepEndBy` newline
    where
    ruleP :: Parser Rule
    ruleP = try (rule 3 4) <|> rule 2 3

    rule :: Int -> Int -> Parser Rule
    rule fromSize toSize = (,) <$> squareP fromSize <* string " => " <*> squareP toSize

    squareP :: Int -> Parser Square
    squareP size = replicateM size anyChar `sepBy` char '/'

mkRules :: [Rule] -> Rules
mkRules parsed = M.fromList [(from',to) | (from,to) <- parsed, from' <- (rotate from ++ rotate (flipVertically from))]

rotate :: [[a]] -> [[[a]]]
rotate = take 4 . iterate clockwise
    where
    clockwise :: [[a]] -> [[a]]
    clockwise = map reverse . transpose

flipVertically :: [[a]] -> [[a]]
flipVertically = map reverse
