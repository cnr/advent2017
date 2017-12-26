
module Main (main) where

import Common
import Data.Monoid
import Data.List
import Data.Ord
import Data.Tree
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Pipe = Pipe { start :: Int
                 , end   :: Int
                 } deriving Show

main :: IO ()
main = do
    pipes <- readParsedLines (Pipe <$> decimal <* char '/' <*> decimal) 24

    let startPipes = [pipe | pipe@((Pipe 0 _),_) <- pick pipes]
    let result = Node (Pipe 0 0) (map (uncurry build) startPipes)

    print (part1 result)
    print (part2 result)

part1 :: Tree Pipe -> Int
part1 = foldTree (\n xs -> n + maximum (0:xs)) . fmap pipeValue

part2 :: Tree Pipe -> Int
part2 = strength . head . sortBy (flip (comparing length) <> flip (comparing strength)) . paths
    where
    strength = sum . map pipeValue

paths :: Tree a -> [[a]]
paths (Node x []) = [[x]]
paths (Node x xs) = map (x:) (paths =<< xs)

build :: Pipe -> [Pipe] -> Tree Pipe
build prev pipes = Node prev [build next' remaining | (next,remaining) <- pick pipes, next' <- [next, flipPipe next], end prev == start next']

flipPipe :: Pipe -> Pipe
flipPipe (Pipe x y) = Pipe y x

pipeValue :: Pipe -> Int
pipeValue (Pipe x y) = x + y

pick :: [a] -> [(a,[a])]
pick []     = []
pick (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- pick xs]
