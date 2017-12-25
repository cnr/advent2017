-- lazy/boring solution today

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Common
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    gs <- mkGridState . lines <$> readInput 22

    print (part1 gs)
    print (part2 gs)

mkGridState :: [[Char]] -> GridState
mkGridState ys = let middle = length ys `div` 2
                  in GridState { posn      = (middle,middle)
                               , direction = North
                               , tiles     = M.fromList [((x,y),Infected) | (y,line) <- zip [0..] ys, (x,c) <- zip [0..] line, c == '#']
                               }

part1 :: GridState -> Int
part1 = countInfections virus 10000
    where
    virus Infected = Clean
    virus Clean    = Infected
    virus Weakened = error "invalid state"
    virus Flagged  = error "invalid state"

part2 :: GridState -> Int
part2 = countInfections virus 10000000
    where
    virus Clean    = Weakened
    virus Weakened = Infected
    virus Infected = Flagged
    virus Flagged  = Clean

countInfections :: Virus -> Int -> GridState -> Int
countInfections virus iterations = getSum . execWriter . runStateT (replicateM_ iterations (step virus))

step :: Virus -> StateT GridState (Writer (Sum Int)) ()
step f = turn *> virus *> move
    where
    turn = do
        GridState{..} <- get
        let tile = tileAt posn tiles
        modify (\gs -> gs { direction = turnForTile tile direction } )

    virus = do
        coords <- gets posn
        tile   <- gets (tileAt coords . tiles)

        let newTile = f tile
        when (newTile == Infected) (tell 1)
        modify (\gs -> gs { tiles = M.insert coords newTile (tiles gs) } )

    move = modify (\gs -> gs { posn = walk (direction gs) (posn gs) } )

    tileAt :: (Int,Int) -> M.Map (Int,Int) Tile -> Tile
    tileAt coords = fromMaybe Clean . M.lookup coords

turnForTile :: Tile -> Direction -> Direction
turnForTile Clean    = turnLeft
turnForTile Weakened = id
turnForTile Infected = turnRight
turnForTile Flagged  = turnLeft . turnLeft

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

walk :: Direction -> (Int,Int) -> (Int,Int)
walk North (x,y) = (x,y-1)
walk South (x,y) = (x,y+1)
walk East  (x,y) = (x+1,y)
walk West  (x,y) = (x-1,y)

type Virus = (Tile -> Tile)

data GridState = GridState { posn      :: (Int,Int)
                           , direction :: Direction
                           , tiles     :: M.Map (Int,Int) Tile
                           } deriving Show

data Direction = North | East | South | West deriving Show

data Tile = Clean
          | Weakened
          | Infected
          | Flagged
          deriving (Eq, Show)
