
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = do
    layers <- map (unsafeParse layerP) . lines <$> readFile "input.txt"
    print (part1 layers)
    print (part2 layers)

type Layer = (Int, Int) -- ix, range

part1 :: [(Int,Int)] -> Int
part1 xs = sum [uncurry (*) layer | layer <- xs, blocked 0 layer]

part2 :: [(Int,Int)] -> Int
part2 xs = head [offset | offset <- [0..], none (blocked offset) xs]

none :: (a -> Bool) -> [a] -> Bool
none f = not . any f

blocked :: Int -- additional offset, if any
        -> Layer
        -> Bool
blocked offset (ix,range) = (ix + offset) `mod` ((range - 1) * 2) == 0


---- Parsing input

layerP :: Parser Layer
layerP = (,) <$> uintP <* string ": " <*> uintP

uintP :: Parser Int
uintP = read <$> some digitChar

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = result where Right result = parse parser "input" input
