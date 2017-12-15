#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import Common
import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = do
    input <- readParsedLines discP 7

    let bottom = part1 input

    print bottom -- part 1
    print (part2 input bottom)

part1 :: [Disc] -> Disc
part1 discs = head [disc | disc <- discs, dName disc `notElem` (dHolding =<< discs)]

part2 :: [Disc] -> Disc -> Int
part2 discs = surface 0
    where

    surface :: Int  -- expected weight
            -> Disc -- current
            -> Int  -- diff to balance
    surface expected disc =
        case imbalanced of
            Just (x,weight) -> surface weight x
            Nothing         -> expected - heldWeight disc + dWeight disc

        where
        held = map discByName (dHolding disc)

        -- disgusting -- need to clean this up
        imbalanced :: Maybe (Disc, Int) -- (imbalanced disc, expected weight)
        imbalanced = case sortBy (comparing length) . groupBy ((==) `on` heldWeight) . sortBy (comparing heldWeight) $ held of
                         (x:y:_) -> Just (head x, heldWeight (head y))
                         _       -> Nothing -- one weight


    discsByName = M.fromList (map (\disc -> (dName disc, disc)) discs)
    discByName = (discsByName M.!)

    heldWeight :: Disc -> Int
    heldWeight (Disc _ weight held) = weight + sum (map (heldWeight . discByName) held)

data Disc = Disc { dName    :: String
                 , dWeight  :: Int
                 , dHolding :: [String]
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
