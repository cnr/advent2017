
module Main (main) where

import Common
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
    insns <- readParsedLines insnP 23

    print (part1 insns)
    print part2

part1 :: [Insn] -> Int
part1 insns = length [() | Mul _ _ <- debug insns]

-- the program essentially boiled down to (imperative pseudocode):
-- 
-- int c = 125100
-- for (int b = 108100; b <= c; b++) {
--     if (b isn't prime) {
--         h++;
--     }
-- }
part2 :: Int
part2 = length [() | b <- [108100,108117..125100], not (isPrime b)]

isPrime :: Int -> Bool
isPrime n = null [() | m <- [2..isqrt n], n `mod` m == 0]

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

debug :: [Insn] -> [Insn]
debug insns = go 0 M.empty
    where
    go :: Int -> Registers -> [Insn]
    go n regs
      | n < 0 || n >= length insns = []
      | otherwise                  =
          let insn        = insns !! n
              binop f r v = go (n+1) (M.insert r (getReg r regs `f` getVal v regs) regs)
           in insn : case insn of
                       Set r v -> binop (const id) r v
                       Sub r v -> binop (-) r v
                       Mul r v -> binop (*) r v
                       Jnz v j ->
                           case getVal v regs of
                             0 -> go (n + 1)             regs
                             _ -> go (n + getVal j regs) regs

getVal :: Val -> Registers -> Int
getVal (IntVal i) = const i
getVal (RegVal c) = getReg c

getReg :: Register -> Registers -> Int
getReg c = fromMaybe 0 . M.lookup c

data Insn = Set Register Val
          | Sub Register Val
          | Mul Register Val
          | Jnz Val      Val
          deriving Show

data Val = RegVal Register
         | IntVal Int
         deriving Show

type Register = Char

type Registers = M.Map Char Int

insnP :: Parser Insn
insnP = Set <$ string "set " <*> regP <* char ' ' <*> valP
    <|> Sub <$ string "sub " <*> regP <* char ' ' <*> valP
    <|> Mul <$ string "mul " <*> regP <* char ' ' <*> valP
    <|> Jnz <$ string "jnz " <*> valP <* char ' ' <*> valP

regP :: Parser Char
regP = oneOf ['a'..'h']

valP :: Parser Val
valP = RegVal <$> regP
   <|> IntVal <$> intP
