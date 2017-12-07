#!/usr/bin/env stack
-- stack runghc

import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

main :: IO ()
main = do
    input <- map (unsafeParse discP) . lines <$> readFile "input.txt"

    let bottom = part1 input

    print bottom -- part 1
    print (part2 input bottom)

part1 :: [Disc] -> Disc
part1 discs = head [disc | disc <- discs, name disc `notElem` (holding =<< discs)]

part2 :: [Disc] -> Disc -> Int
part2 discs = surface 0
    where

    surface :: Int  -- expected weight
            -> Disc -- current
            -> Int  -- diff to balance
    surface expected disc =
        case imbalanced of
            Just (x,weight) -> surface weight x
            Nothing         -> expected - heldWeight disc + weight disc

        where
        held = map discByName (holding disc)

        -- disgusting -- need to clean this up
        imbalanced :: Maybe (Disc, Int) -- (imbalanced disc, expected weight)
        imbalanced = case sortBy (comparing length) . groupBy ((==) `on` heldWeight) . sortBy (comparing heldWeight) $ held of
                         [_] -> Nothing
                         (x:y:_) -> Just (head x, heldWeight (head y))


    discsByName = M.fromList (map (\disc -> (name disc, disc)) discs)
    discByName = (discsByName M.!)

    heldWeight :: Disc -> Int
    heldWeight (Disc _ weight held) = weight + sum (map (heldWeight . discByName) held)

data Disc = Disc { name    :: String
                 , weight  :: Int
                 , holding :: [String]
                 } deriving Show

discP :: Parser Disc
discP = Disc <$> some letterChar
             <*  spaceChar
             <*  char '('
             <*> (read <$> some digitChar)
             <*  char ')'
             <*> (fromMaybe [] <$> optional holding)
    where
    holding = string " -> " *> some letterChar `sepBy1` string ", "

unsafeParse :: Parser a -> String -> a
unsafeParse parser xs = result where Right result = parse parser "source" xs
