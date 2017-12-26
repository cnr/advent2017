
-- TODO: derive the part 2 solution as part of the function, instead of hardcoding our own

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Common
import qualified Data.Bimap as B
import           Data.List
import           Data.Ord
import           Text.Megaparsec
import           Text.Megaparsec.Char

main :: IO ()
main = do
    insns <- readParsed (insnP `sepBy` char ',') 16
    print (part1 insns)
    print (part2 insns)

initial :: B.Bimap Int Char
initial = B.fromList (zip [0..] ['a'..'p'])

data Insn = Spin Int
          | Exchange Int Int
          | Partner Char Char
          deriving Show

part1 :: [Insn] -> [Char]
part1 = extract . foldl' step initial

part2 :: [Insn] -> [Char]
part2 = part1 . concat . replicate 16

step :: B.Bimap Int Char -> Insn -> B.Bimap Int Char
step programs = \case
    Spin     n   -> B.map (\ix -> (ix + n) `mod` B.size programs) $! programs
    Exchange x y -> let c = programs B.! x
                        d = programs B.! y
                     in B.insert x d . B.insert y c $! programs
    Partner  c d -> let x = programs B.!> c
                        y = programs B.!> d
                     in B.insert x d . B.insert y c $! programs

extract :: B.Bimap Int Char -> [Char]
extract = map snd . sortBy (comparing fst) . B.assocs

-----------------------------

insnP :: Parser Insn
insnP = Spin     <$ char 's' <*> uintP
    <|> Exchange <$ char 'x' <*> uintP <* char '/' <*> uintP
    <|> Partner  <$ char 'p' <*> programP <* char '/' <*> programP

programP :: Parser Char
programP = oneOf ['a'..'p']

uintP :: Parser Int
uintP = read <$> some digitChar
