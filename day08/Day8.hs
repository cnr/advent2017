#!/usr/bin/env stack
-- stack runghc

import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.String

type Registers = M.Map String Int

data Insn = Insn { target    :: String
                 , delta     :: Int
                 , condition :: Registers -> Bool
                 }

main :: IO ()
main = do
    insns <- map (unsafeParse insnP) . lines <$> readFile "input.txt"

    let results = scanl step M.empty insns

    print $ maximum (last results)
    print $ maximum (toList =<< results)

step :: Registers -> Insn -> Registers
step registers (Insn target delta condition)
  | condition registers = M.insertWith (+) target delta registers
  | otherwise           = registers


---- Parsing input

insnP :: Parser Insn
insnP = Insn <$> some letterChar <* spaceChar <*> deltaP <* string " if " <*> conditionP

deltaP :: Parser Int
deltaP = string "inc " *> intP
     <|> string "dec " *> (negate <$> intP)

conditionP :: Parser (Registers -> Bool)
conditionP = do
    reg  <- some letterChar
    spaceChar
    func <- compP
    spaceChar
    value <- intP
    return (\registers -> fromMaybe 0 (M.lookup reg registers) `func` value)

compP :: Parser (Int -> Int -> Bool)
compP = (/=) <$ string "!="
    <|> (==) <$ string "=="
    <|> (>=) <$ string ">="
    <|> (<=) <$ string "<="
    <|> (>)  <$ char '>'
    <|> (<)  <$ char '<'

intP :: Parser Int
intP = negate <$ char '-' <*> (read <$> some digitChar)
   <|>                         read <$> some digitChar

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = result where Right result = parse parser "input" input
