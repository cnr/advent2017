#!/usr/bin/env stack
-- stack runghc

module Main (main) where

import           Common
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Registers = M.Map String Int

data Insn = Insn { target    :: String
                 , delta     :: Int
                 , condition :: Registers -> Bool
                 }

main :: IO ()
main = do
    insns <- readParsedLines insnP 8

    let results = scanl step M.empty insns

    print $ maximum (last results)
    print $ maximum (toList =<< results)

step :: Registers -> Insn -> Registers
step registers insn
  | (condition insn) registers = M.insertWith (+) (target insn) (delta insn) registers
  | otherwise                  = registers


---- Parsing input

insnP :: Parser Insn
insnP = Insn <$> some letterChar <* spaceChar <*> deltaP <* string " if " <*> conditionP

deltaP :: Parser Int
deltaP = string "inc " *> intP
     <|> string "dec " *> (negate <$> intP)

conditionP :: Parser (Registers -> Bool)
conditionP = do
    reg   <- some letterChar
    _     <- spaceChar
    func  <- compP
    _     <- spaceChar
    value <- intP
    return (\registers -> fromMaybe 0 (M.lookup reg registers) `func` value)

compP :: Parser (Int -> Int -> Bool)
compP = (/=) <$ string "!="
    <|> (==) <$ string "=="
    <|> (>=) <$ string ">="
    <|> (<=) <$ string "<="
    <|> (>)  <$ char '>'
    <|> (<)  <$ char '<'
