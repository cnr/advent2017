#!/usr/bin/env stack
-- stack runghc

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

main :: IO ()
main = do
    input <- M.fromList . map (unsafeParse pipesP) . lines <$> readFile "input.txt"
    print (part1 input)
    print (part2 input)
    return ()

part1 :: M.Map Int [Int] -> Int
part1 pipes = length (dfs (pipes M.!) 0)

part2 :: M.Map Int [Int] -> Int
part2 = length . go
    where
    go :: M.Map Int [Int] -> [[Int]] -- find all groups
    go pipes
      | M.null pipes = []
      | otherwise    = let (seed,_) = M.findMin pipes
                           group    = dfs (pipes M.!) seed
                        in group : go (foldr M.delete pipes group)

dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs f seed = go S.empty [seed]
    where
    go _    [] = []
    go seen (x:xs)
      | S.member x seen = go seen xs
      | otherwise = x : go (S.insert x seen) (f x ++ xs)

pipesP :: Parser (Int, [Int])
pipesP = (,) <$> uintP <* string " <-> " <*> (uintP `sepBy` string ", ")

uintP :: Parser Int
uintP = read <$> some digitChar

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = result where Right result = parse parser "input" input
