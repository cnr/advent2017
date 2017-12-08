#!/usr/bin/env stack
-- stack runghc

import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.String

type Registers = M.Map String Int

data Insn = Insn { target    :: String
                 , action    :: Int -> Int
                 , condition :: Registers -> Bool
                 }

main :: IO ()
main = do
    insns <- map (unsafeParse insnP) . lines <$> readFile "input.txt"

    print $ maximum (foldl step M.empty insns) -- part 1
    print $ maximum (toList =<< scanl step M.empty insns) -- part 2

step :: Registers -> Insn -> Registers
step registers (Insn target action condition)
  | condition registers = M.insertWith (+) target (action 0) registers
  | otherwise           = registers


---- Parsing input

insnP :: Parser Insn
insnP = Insn <$> some letterChar <* spaceChar <*> actionP <* string " if " <*> conditionP

actionP :: Parser (Int -> Int)
actionP = (+)      <$ string "inc " <*> intP
      <|> subtract <$ string "dec " <*> intP

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
