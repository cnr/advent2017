#!/usr/bin/env stack
-- stack runghc

main :: IO ()
main = do
    input <- map (map read . words) . lines <$> readFile "input.txt"
    print (checksum1 input)
    print (checksum2 input)
    return ()

checksum1 :: [[Int]] -> Int
checksum1 = sum . map difference
    where
    difference xs = maximum xs - minimum xs

-- point-free for fun
checksum1' :: [[Int]] -> Int
checksum1' = sum . map ((-) <$> maximum <*> minimum)

checksum2 :: [[Int]] -> Int
checksum2 = sum . map divisible
    where
    divisible xs = head [x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0]
