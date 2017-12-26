
module Main (main) where

import Common
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
    insns <- V.fromList <$> readParsedLines insnP 18
        
    print (part1 insns)
    print (part2 (program 0 insns) (program 1 insns))

part1 :: V.Vector Insn -> Int
part1 insns = go 0 M.empty 0
    where
    go :: Int -> Registers -> Int -> Int
    go n regs latest =
        case insns V.!? n of
            Nothing            -> error ("last: " ++ show latest)
            Just (Snd val)     -> go (n+1) regs (getValue val regs)
            Just (Set reg val) -> binop (const id) reg val
            Just (Add reg val) -> binop (+) reg val
            Just (Mul reg val) -> binop (*) reg val
            Just (Mod reg val) -> binop mod reg val
            Just (Rcv reg)     -> if getRegister reg regs /= 0 then latest else go (n+1) regs latest
            Just (Jgz val jmp) -> if getValue val regs > 0 then go (n + (getValue jmp regs)) regs latest else go (n+1) regs latest

        where
        binop :: (Int -> Int -> Int) -> Register -> Val -> Int
        binop f reg val = go (n+1) (M.insert reg (getRegister reg regs `f` getValue val regs) regs) latest

data Insn = Snd Val
          | Set Register Val -- to from
          | Add Register Val -- to from
          | Mul Register Val -- to from
          | Mod Register Val -- to from
          | Rcv Register
          | Jgz Val Val
          deriving Show

type Register = Char

type Registers = M.Map Char Int

getRegister :: Char -> Registers -> Int
getRegister c = fromMaybe 0 . M.lookup c

getValue :: Val -> Registers -> Int
getValue (RegVal reg) regs = getRegister reg regs
getValue (IntVal val) _    = val

data Val = RegVal Register
         | IntVal Int
         deriving Show

-------- Part 2

part2 :: Program -> Program -> Int
part2 prog0 prog1 = length $ runLeft [] (unFix prog0) (unFix prog1)
    where
    
    runLeft :: [Int] -> Stopped -> Stopped -> [Int]
    runLeft xs p1 p2 =
        case exec xs p1 of
            Request [] _   -> []
            Request ys p1' -> runRight ys (Request [] p1') p2

    runRight :: [Int] -> Stopped -> Stopped -> [Int]
    runRight ys p1 p2 =
        case exec ys p2 of
            Request [] _   -> []
            Request xs p2' -> xs ++ runLeft xs p1 (Request [] p2') -- note the (xs ++) to record the solution

    exec :: [Int] -> Stopped -> Stopped
    exec [] prog = prog
    exec (x:xs) (Request out prog) = let (Request out' prog') = unFix (prog x)
                                      in exec xs (Request (out ++ out') prog')

program :: Int -> V.Vector Insn -> Program
program pId insns = go 0 [] (M.singleton 'p' pId)
    where
        go :: Int -> [Int] -> Registers -> Program
        go n output regs = case insns V.! n of
            Snd val     -> go (n+1) (output ++ [getValue val regs]) regs
            Set reg val -> binop (const id) reg val
            Add reg val -> binop (+) reg val
            Mul reg val -> binop (*) reg val
            Mod reg val -> binop mod reg val
            Rcv reg     -> Fix (Request output (go (n+1) [] . set reg))
            Jgz val jmp -> go (n + if getValue val regs > 0 then getValue jmp regs else 1) output regs

            where
            set :: Register -> Int -> Registers
            set reg val = M.insert reg val regs

            binop :: (Int -> Int -> Int) -> Register -> Val -> Program
            binop f reg val = go (n+1) output (set reg (getRegister reg regs `f` getValue val regs))

type Program = Fix (Request [Int] Int)

type Stopped = Request [Int] Int Program

newtype Fix f = Fix { unFix :: f (Fix f) }

data Request a r b = Request a (r -> b)


------ Parsers

insnP :: Parser Insn
insnP = Snd <$ string "snd " <*> valP
    <|> Set <$ string "set " <*> regP <* char ' ' <*> valP
    <|> Add <$ string "add " <*> regP <* char ' ' <*> valP
    <|> Mul <$ string "mul " <*> regP <* char ' ' <*> valP
    <|> Mod <$ string "mod " <*> regP <* char ' ' <*> valP
    <|> Rcv <$ string "rcv " <*> regP
    <|> Jgz <$ string "jgz " <*> valP <* char ' ' <*> valP

regP :: Parser Register
regP = letterChar

valP :: Parser Val
valP = RegVal <$> regP
   <|> IntVal <$> intP
